(ns chess.selfplay
  (:require [chess.action :as action]
            [chess.az :as az]
            [chess.basics :as b]
            [chess.encoding :as enc]
            [chess.native :as native]
            [chess.nn :as nn])
  (:import [java.util.concurrent Callable Executors ExecutorCompletionService TimeUnit]))

(def ^:const default-max-plies 200)
(def ^:const progress-plies 25)

(defn available-threads []
  (.availableProcessors (Runtime/getRuntime)))

(defn- examples-from-history
  [history outcome-fn]
  (vec (map-indexed
        (fn [_ {:keys [board policy stm]}]
          (nn/example-from-position board policy (outcome-fn stm)))
        history)))

(defn- pick-move-idx
  [policy best-idx temperature board]
  (if (and best-idx (< temperature 1.0e-6))
    best-idx
    (let [legal (action/legal-move-indices board)
          weights (map #(aget policy %) legal)
          total (reduce + 0.0 weights)]
      (if (zero? total)
        (or best-idx (first legal))
        (let [r (* total (rand))]
          (loop [legal' legal, weights' weights, acc 0.0]
            (if (empty? legal')
              (or best-idx (first legal))
              (let [w (first weights')]
                (if (>= (+ acc w) r)
                  (first legal')
                  (recur (rest legal') (rest weights') (+ acc w)))))))))))

(defn play-game
  "Return training examples {:features :target-policy :target-value :legal-indices}."
  [net {:keys [simulations temperature c-puct game-id max-plies deadline-ms]
        :or {simulations 100, temperature 1.0, c-puct az/default-c-puct
             max-plies default-max-plies}}]
  (letfn [(timed-out? [] (and deadline-ms (>= (System/currentTimeMillis) deadline-ms)))]
    (loop [board b/start-board, history [], ply 0]
      (if (timed-out?)
        (examples-from-history history (constantly 0.0))
        (if-let [tv (enc/terminal-value board)]
          (examples-from-history history #(if (= % "w") tv (- tv)))
          (if (>= ply max-plies)
            (examples-from-history history (constantly 0.0))
            (do
              (when (and game-id (zero? (mod ply progress-plies)))
                (println "  game" game-id "ply" ply))
              (let [{:keys [policy best-idx]}
                    (az/search board net {:simulations simulations
                                          :c-puct c-puct
                                          :deadline deadline-ms})
                    move-idx (pick-move-idx policy best-idx temperature board)
                    move (action/index->move board move-idx)]
                (if-some [move move]
                  (recur (b/move-piece board (first move) (second move))
                         (conj history {:board board
                                        :policy policy
                                        :stm (:side-to-move (board 64))})
                         (inc ply))
                  (examples-from-history history (constantly 0.0)))))))))))

(defn- game-callable
  [net opts game-id]
  (reify Callable
    (call [_]
      (play-game net (assoc opts :game-id game-id)))))

(defn- generate-games-sequential
  [net n opts]
  (vec (mapcat (fn [i]
                 (let [examples (play-game net (assoc opts :game-id (inc i)))]
                   (println "  game" (inc i) "/" n "finished:"
                            (count examples) "positions")
                   examples))
               (range n))))

(defn- generate-games-parallel
  [net n opts thread-count]
  (let [pool (Executors/newFixedThreadPool thread-count)
        completion (ExecutorCompletionService. pool)
        deadline-ms (:deadline-ms opts)
        time-up? #(and deadline-ms (>= (System/currentTimeMillis) deadline-ms))
        t0 (System/currentTimeMillis)
        done (atom 0)]
    (try
      (do
        (doseq [i (range n)]
          (.submit ^ExecutorCompletionService completion
                   ^Callable (game-callable net opts (inc i))))
        (loop [results [], completed 0]
          (cond
            (= completed n) (vec (mapcat identity results))
            (time-up?) (do
                         (println "  time limit reached after" completed "of" n "games")
                         (.shutdownNow pool)
                         (vec (mapcat identity results)))
            :else (if-let [f (.poll completion 5 TimeUnit/SECONDS)]
                    (let [examples (.get f)
                          n-done (swap! done inc)
                          elapsed (/ (- (System/currentTimeMillis) t0) 1000.0)]
                      (println "  game" n-done "/" n "finished:"
                               (count examples) "positions ("
                               (format "%.0f" elapsed) "s elapsed)")
                      (recur (conj results examples) (inc completed)))
                    (recur results completed)))))
      (finally
        (when-not (.isShutdown pool) (.shutdown pool))
        (.awaitTermination pool 1 TimeUnit/SECONDS)
        (native/shutdown!)))))

(defn generate-games
  "Generate n self-play games. Use :threads for parallelism (default: CPU count)."
  [net n & {:keys [simulations temperature c-puct threads max-plies deadline-ms]
            :or {simulations 100, temperature 1.0, c-puct az/default-c-puct}}]
  (let [opts {:simulations simulations
              :temperature temperature
              :c-puct c-puct
              :max-plies (or max-plies default-max-plies)
              :deadline-ms deadline-ms}
        thread-count (max 1 (or threads (available-threads)))]
    (if (= thread-count 1)
      (generate-games-sequential net n opts)
      (generate-games-parallel net n opts thread-count))))

(defn estimate-work
  [{:keys [games simulations max-plies threads]}]
  (let [plies (or max-plies default-max-plies)
        sims (* games plies simulations)
        secs (int (* sims 0.08 (/ games (max 1 threads))))]
    {:total-sims sims
     :estimated-minutes (max 1 (quot secs 60))}))
