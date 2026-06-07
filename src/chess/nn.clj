(ns chess.nn
  "Dual-head policy/value network (dense, AlphaZero-style)."
  (:require [chess.action :as action]
            [chess.encoding :as enc]
            [clojure.java.io :as io])
  (:import [java.io PushbackInputStream]))

(def ^:const input-size enc/feature-size)
(def ^:const hidden-size 128)
(def ^:const value-hidden-size 64)
(def ^:const policy-size action/action-size)

(defn- xavier-scale [fan-in fan-out]
  (Math/sqrt (/ 6.0 (+ fan-in fan-out))))

(defn- rand-weights [rows cols]
  (let [scale (xavier-scale cols rows)]
    (float-array (repeatedly (* rows cols) #(- (* 2 scale (rand)) scale)))))

(defn- zeros [n]
  (float-array n))

(defn new-network []
  {:w1 (rand-weights hidden-size input-size)
   :b1 (zeros hidden-size)
   :w-policy (rand-weights policy-size hidden-size)
   :b-policy (zeros policy-size)
   :w-value (rand-weights value-hidden-size hidden-size)
   :b-value (zeros value-hidden-size)
   :w-out (rand-weights 1 value-hidden-size)
   :b-out (zeros 1)})

(defn- at [^floats arr row col row-width]
  (aget arr (+ col (* row row-width))))

(defn- aset2d!
  [^floats arr row col row-width value]
  (aset ^floats arr (+ col (* row row-width)) (float value)))

(defn- linear!
  [^floats out ^floats weights ^floats bias rows cols ^floats input]
  (dotimes [r rows]
    (let [offset (* r cols)]
      (aset out r (aget bias r))
      (dotimes [c cols]
        (aset out r (+ (aget out r) (* (aget input c) (aget weights (+ c offset)))))))))

(defn- relu! [^floats xs]
  (dotimes [i (alength xs)]
    (when (neg? (aget xs i))
      (aset xs i 0.0))))

(defn- relu-d [x]
  (if (pos? x) 1.0 0.0))

(defn- softmax-masked
  [^floats logits legal-indices]
  (let [out (zeros (alength logits))
        max-logit (apply max (map #(aget logits %) legal-indices))
        sum (reduce +
                    (map (fn [i]
                           (let [e (float (Math/exp (- (aget logits i) max-logit)))]
                             (aset ^floats out i e)
                             e))
                         legal-indices))]
    (doseq [i legal-indices]
      (aset ^floats out i (float (/ (aget out i) sum))))
    out))

(defn forward
  [net features legal-indices]
  (let [{:keys [w1 b1 w-policy b-policy w-value b-value w-out b-out]} net
        hidden (zeros hidden-size)
        policy-logits (zeros policy-size)
        value-hidden (zeros value-hidden-size)
        value-out (zeros 1)]
    (linear! hidden w1 b1 hidden-size input-size features)
    (relu! hidden)
    (linear! policy-logits w-policy b-policy policy-size hidden-size hidden)
    (linear! value-hidden w-value b-value value-hidden-size hidden-size hidden)
    (relu! value-hidden)
    (linear! value-out w-out b-out 1 value-hidden-size value-hidden)
    (let [value (Math/tanh (aget value-out 0))
          policy (if (seq legal-indices)
                   (softmax-masked policy-logits legal-indices)
                   policy-logits)]
      {:hidden hidden
       :value-hidden value-hidden
       :policy-logits policy-logits
       :policy policy
       :value value})))

(defn predict [net board]
  (let [features (enc/board->features board)
        legal (action/legal-move-indices board)]
    (forward net features legal)))

(defn- net->serializable [net]
  (into {} (map (fn [[k v]] [k (vec v)]) net)))

(defn- serializable->net [data]
  (into {} (map (fn [[k v]] [k (float-array v)]) data)))

(defn save-network! [net path]
  (.mkdirs (.getParentFile (io/file path)))
  (spit path (pr-str (net->serializable net))))

(defn load-network [path]
  (serializable->net (read-string (slurp path))))

(defn default-model-path []
  "models/network.edn")

(defn load-or-create []
  (let [path (default-model-path)]
    (if (.exists (io/file path))
      (load-network path)
      (new-network))))

(defn train-step!
  [net {:keys [features target-policy target-value legal-indices]} lr]
  (let [{:keys [w1 b1 w-policy b-policy w-value b-value w-out b-out]} net
        {:keys [hidden value-hidden policy value]} (forward net features legal-indices)
        d-logits (zeros policy-size)
        d-hidden (zeros hidden-size)
        value-err (- value target-value)
        tanh-deriv (- 1.0 (* value value))]
    (doseq [i legal-indices]
      (aset ^floats d-logits i (float (- (or (aget policy i) 0.0)
                                           (or (aget target-policy i) 0.0)))))
    (dotimes [h hidden-size]
      (aset ^floats d-hidden h
            (float (+ (reduce + (for [i legal-indices]
                                  (* (aget d-logits i) (at w-policy i h hidden-size))))
                     (reduce + (for [vh (range value-hidden-size)]
                                 (* value-err tanh-deriv (aget w-out vh)
                                    (relu-d (aget value-hidden vh))
                                    (at w-value vh h hidden-size))))))))
    (doseq [i legal-indices]
      (aset ^floats b-policy i (float (- (aget b-policy i) (* lr (aget d-logits i)))))
      (dotimes [h hidden-size]
        (aset2d! w-policy i h hidden-size
                 (float (- (at w-policy i h hidden-size)
                           (* lr (aget d-logits i) (aget hidden h)))))))
    (dotimes [vh value-hidden-size]
      (let [d-vh (* value-err tanh-deriv (aget w-out vh))]
        (aset ^floats b-value vh (float (- (aget b-value vh) (* lr d-vh (relu-d (aget value-hidden vh))))))
        (dotimes [h hidden-size]
          (aset2d! w-value vh h hidden-size
                   (float (- (at w-value vh h hidden-size)
                             (* lr d-vh (relu-d (aget value-hidden vh)) (aget hidden h))))))))
    (aset ^floats b-out 0 (float (- (aget b-out 0) (* lr value-err))))
    (dotimes [vh value-hidden-size]
      (aset ^floats w-out vh (float (- (aget w-out vh) (* lr value-err tanh-deriv (aget value-hidden vh))))))
    (dotimes [h hidden-size]
      (aset ^floats b1 h (float (- (aget b1 h) (* lr (aget d-hidden h)))))
      (dotimes [i input-size]
        (aset2d! w1 h i input-size
                 (float (- (at w1 h i input-size)
                           (* lr (aget d-hidden h) (aget features i)))))))
    net))

(defn train-batch! [net examples lr]
  (reduce (fn [n ex] (train-step! n ex lr)) net examples))

(defn example-from-position [board target-policy target-value]
  {:features (enc/board->features board)
   :target-policy target-policy
   :target-value (float target-value)
   :legal-indices (action/legal-move-indices board)})
