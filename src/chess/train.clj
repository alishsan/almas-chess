(ns chess.train
  (:require [chess.native :as native]
            [chess.nn :as nn]
            [chess.selfplay :as selfplay]
            [clojure.string :as str]))

(def ^:const default-learning-rate 0.001)
(def ^:const default-replay-size 5000)

(defn- take-shuffled [n coll]
  (take n (shuffle coll)))

(defn- deadline-ms
  [minutes start-ms]
  (when (and minutes (pos? minutes))
    (+ start-ms (* minutes 60 1000))))

(defn train-iteration!
  [{:keys [net games simulations batch-size learning-rate replay-buffer threads max-plies deadline-ms]
    :or {games 2
         simulations 50
         batch-size 16
         learning-rate default-learning-rate
         replay-buffer (atom [])}}]
  (let [thread-count (max 1 (or threads (selfplay/available-threads)))
        {:keys [estimated-minutes total-sims]}
        (selfplay/estimate-work {:games games
                                 :simulations simulations
                                 :max-plies max-plies
                                 :threads thread-count})]
    (println "Self-play:" games "game(s)," simulations "MCTS sims/move,"
             thread-count "thread(s), max" (or max-plies selfplay/default-max-plies) "plies/game")
    (println "  (~" total-sims "MCTS sims, est." estimated-minutes "+ min — progress logged per game)")
    (let [t0 (System/currentTimeMillis)
          examples (selfplay/generate-games net games
                                            :simulations simulations
                                            :threads thread-count
                                            :max-plies max-plies
                                            :deadline-ms deadline-ms)
          elapsed (/ (- (System/currentTimeMillis) t0) 1000.0)]
      (println "  positions:" (count examples) "(in" (format "%.0f" elapsed) "s)")
      (if (empty? examples)
        (do (println "  no examples collected, skipping training") net)
        (let [buffer' (swap! replay-buffer
                             (fn [buf]
                               (take-shuffled default-replay-size (concat buf examples))))
              batch (take-shuffled batch-size buffer')
              net' (nn/train-batch! net batch learning-rate)]
          (println "  trained on batch of" (count batch))
          net')))))

(defn train!
  [& {:keys [iterations games simulations batch-size learning-rate model-path threads max-plies minutes]
      :or {iterations 5
           games 2
           simulations 50
           batch-size 16
           learning-rate default-learning-rate
           model-path (nn/default-model-path)}}]
  (let [start-ms (System/currentTimeMillis)
        limit-ms (deadline-ms minutes start-ms)
        time-up? (fn [] (and limit-ms (>= (System/currentTimeMillis) limit-ms)))]
    (println "AlphaZero training")
    (native/print-backend-status!)
    (println "  iterations:" iterations)
    (println "  model:" model-path)
    (println "  self-play threads:" (max 1 (or threads (selfplay/available-threads))))
    (when minutes (println "  time limit:" minutes "minutes"))
    (let [replay (atom [])
          net (loop [net (nn/load-or-create)
                     i 1]
                (if (or (> i iterations) (time-up?))
                  net
                  (do
                    (println "\n=== Iteration" i "===")
                    (let [net' (train-iteration!
                                {:net net
                                 :games games
                                 :simulations simulations
                                 :batch-size batch-size
                                 :learning-rate learning-rate
                                 :replay-buffer replay
                                 :threads threads
                                 :max-plies max-plies
                                 :deadline-ms limit-ms})]
                      (nn/save-network! net' model-path)
                      (println "  saved" model-path)
                      (recur net' (inc i))))))]
      (when (time-up?)
        (println "\nTime limit reached — training stopped."))
      net)))

(defn -main
  [& args]
  (let [opts (apply hash-map args)
        parse-int (fn [k default]
                    (if-let [v (get opts k)]
                      (Integer/parseInt (str v))
                      default))]
    (train!
     :iterations (parse-int ":iterations" 5)
     :games (parse-int ":games" 2)
     :simulations (parse-int ":simulations" 50)
     :batch-size (parse-int ":batch" 16)
     :threads (parse-int ":threads" (selfplay/available-threads))
     :max-plies (parse-int ":max-plies" selfplay/default-max-plies)
     :minutes (parse-int ":minutes" 0))))
