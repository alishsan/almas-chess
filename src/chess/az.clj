(ns chess.az
  "AlphaZero-style MCTS with PUCT and neural network priors/values."
  (:require [chess.action :as action]
            [chess.basics :as b]
            [chess.encoding :as enc]
            [chess.nn :as nn]))

(def ^:const default-c-puct 1.5)

(defn- empty-node []
  {:visit-count 0 :value-sum 0.0 :prior 0.0 :expanded? false :children {}})

(defn- q-value [node]
  (if (pos? (:visit-count node 0))
    (/ (:value-sum node) (:visit-count node))
    0.0))

(defn- terminal-value [board]
  (when-let [v (enc/terminal-value board)]
    (enc/perspective-value board v)))

(defn- apply-move [board move-idx]
  (when-let [move (action/index->move board move-idx)]
    (b/move-piece board (first move) (second move))))

(defn- select-move-idx
  [node c-puct]
  (let [total (reduce + (map :visit-count (vals (:children node))))
        sqrt-total (Math/sqrt (max 1 total))]
    (first (apply max-key
                  (fn [[_idx child]]
                    (+ (q-value child)
                       (* c-puct (:prior child)
                          (/ sqrt-total (inc (:visit-count child))))))
                  (:children node)))))

(defn- expand!
  [tree path board net]
  (let [node (get tree path (empty-node))
        legal (action/legal-move-indices board)]
    (if (empty? legal)
      [tree (or (terminal-value board) 0.0)]
      (let [{:keys [policy value]} (nn/predict net board)
            children (into {}
                           (for [idx legal
                                 :let [p (max 1.0e-8 (aget policy idx))]]
                             [idx (assoc (empty-node) :prior p)]))
            expanded (assoc node :expanded? true :children children)]
        [(assoc tree path expanded) value]))))

(defn- backup
  [tree path leaf-value]
  (loop [t tree
         p (vec path)
         v leaf-value]
    (if (empty? p)
      t
      (let [node (get t p (empty-node))
            updated (assoc node
                           :visit-count (inc (:visit-count node))
                           :value-sum (+ (:value-sum node) v))]
        (recur (assoc t p updated) (pop p) (- v))))))

(defn simulate-once
  [tree board net c-puct]
  (loop [path []
         board board
         tree tree]
    (if-let [tv (terminal-value board)]
      (backup tree path tv)
      (let [node (get tree path (empty-node))]
        (if (not (:expanded? node))
          (let [[tree' leaf-v] (expand! tree path board net)]
            (backup tree' path leaf-v))
          (let [move-idx (select-move-idx node c-puct)
                board' (apply-move board move-idx)]
            (if board'
              (recur (conj path move-idx) board' tree)
              (backup tree path 0.0))))))))

(defn search
  [board net {:keys [simulations c-puct deadline stop?]
              :or {simulations 800 c-puct default-c-puct}}]
  (let [legal (action/legal-move-indices board)]
    (cond
      (empty? legal) {:tree {} :policy (float-array action/action-size) :best-idx nil}
      (= (count legal) 1) {:tree {} :policy (doto (float-array action/action-size)
                                                  (aset (first legal) 1.0))
                           :best-idx (first legal)}
      :else
      (loop [tree {}
             n 0]
        (if (and (< n simulations)
                 (or (nil? deadline) (< (System/currentTimeMillis) deadline))
                 (or (nil? stop?) (not (stop?))))
          (recur (simulate-once tree board net c-puct) (inc n))
          (let [root (get tree [] (empty-node))
                visits (reduce + 0 (map :visit-count (vals (:children root))))
                policy (float-array action/action-size)]
            (doseq [[idx child] (:children root)]
              (when (pos? visits)
                (aset ^floats policy idx (float (/ (:visit-count child) visits)))))
            {:tree tree
             :policy policy
             :best-idx (when (pos? visits)
                         (first (apply max-key (fn [[idx _]] (aget policy idx))
                                             (:children root))))}))))))

(defn best-uci-move
  [board net opts]
  (let [{:keys [best-idx policy]} (search board net opts)
        legal (action/legal-move-indices board)
        idx (or best-idx
                (when (seq legal)
                  (apply max-key #(aget policy %) legal)))]
    (when idx
      (action/move->uci (action/index->move board idx)))))

(defn root-policy-distribution
  [board net opts]
  (:policy (search board net opts)))
