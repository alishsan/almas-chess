(ns chess.core
  (:require [chess.action :as action]
            [chess.az :as az]
            [chess.basics :as b]
            [chess.native :as native]
            [chess.nn :as nn]
            [chess.train :as train]
            [clojure.string :as str])
  (:import [java.util.concurrent LinkedBlockingQueue TimeUnit])
  (:gen-class))

(def engine-name "almas")
(def engine-author "Alisher Sanetullaev")

(def stop-search (atom false))
(def command-queue (LinkedBlockingQueue.))
(defonce network (atom nil))

(defn load-network! []
  (reset! network (nn/load-or-create)))

(defn start-input-reader []
  (future
    (loop []
      (when-let [line (read-line)]
        (.put command-queue line)
        (recur)))))

(defn drain-commands! []
  (loop []
    (when (.poll command-queue 0 TimeUnit/MILLISECONDS)
      (recur))))

(defn stop-requested? []
  (when-let [cmd (.peek command-queue)]
    (when (= "stop" (str/lower-case (str/trim cmd)))
      (.poll command-queue)
      (reset! stop-search true)
      true)))

;; --- Position parsing -------------------------------------------------------

(defn parse-position-line
  [input]
  (let [tokens (str/split (str/trim input) #"\s+")]
    (when (= (first tokens) "position")
      (let [toks (rest tokens)]
        (cond
          (= (first toks) "startpos")
          (let [rest (rest toks)
                moves (if (= (first rest) "moves") (vec (rest rest)) [])]
            {:kind :startpos :moves moves})

          (= (first toks) "fen")
          (let [fen-parts (take 6 (rest toks))
                after (drop 6 (rest toks))
                moves (if (= (first after) "moves") (vec (rest after)) [])]
            {:kind :fen :fen (str/join " " fen-parts) :moves moves})

          :else nil)))))

(defn position-state->board
  [{:keys [kind fen moves]}]
  (let [base (case kind
               :startpos b/start-board
               :fen (b/fen-to-board fen))]
    (reduce b/make-move base moves)))

(defn side-to-move
  [position-state]
  (let [white-to-move? (case (:kind position-state)
                         :startpos (even? (count (:moves position-state)))
                         :fen (let [stm (second (str/split (:fen position-state) #" "))]
                                (if (= stm "w")
                                  (even? (count (:moves position-state)))
                                  (odd? (count (:moves position-state))))))]
    (if white-to-move? :white :black)))

;; --- Search -----------------------------------------------------------------

(def default-search-ms 5000)
(def default-mcts-sims 400)

(defn parse-go
  [input]
  (let [tokens (str/split (str/trim input) #"\s+")]
    (loop [opts {} toks (rest tokens)]
      (if (empty? toks)
        opts
        (let [tok (first toks)]
          (cond
            (= tok "wtime") (recur (assoc opts :wtime (Long/parseLong (second toks))) (drop 2 toks))
            (= tok "btime") (recur (assoc opts :btime (Long/parseLong (second toks))) (drop 2 toks))
            (= tok "winc") (recur (assoc opts :winc (Long/parseLong (second toks))) (drop 2 toks))
            (= tok "binc") (recur (assoc opts :binc (Long/parseLong (second toks))) (drop 2 toks))
            (= tok "movestogo") (recur (assoc opts :movestogo (Long/parseLong (second toks))) (drop 2 toks))
            (= tok "movetime") (recur (assoc opts :movetime (Long/parseLong (second toks))) (drop 2 toks))
            (= tok "depth") (recur (assoc opts :depth (Long/parseLong (second toks))) (drop 2 toks))
            (= tok "nodes") (recur (assoc opts :nodes (Long/parseLong (second toks))) (drop 2 toks))
            (= tok "infinite") (recur (assoc opts :infinite true) (rest toks))
            :else (recur opts (rest toks))))))))

(defn compute-time-ms
  [go-opts position-state]
  (cond
    (:movetime go-opts) (:movetime go-opts)
    (:infinite go-opts) Long/MAX_VALUE
    (and (:wtime go-opts) (:btime go-opts))
    (let [stm (side-to-move position-state)
          time (if (= stm :white) (:wtime go-opts) (:btime go-opts))
          inc-ms (if (= stm :white) (or (:winc go-opts) 0) (or (:binc go-opts) 0))
          mtg (max 1 (or (:movestogo go-opts) 30))]
      (max 100 (int (+ inc-ms (/ time mtg)))))
    :else default-search-ms))

(defn run-search
  [position-state go-opts]
  (reset! stop-search false)
  (drain-commands!)
  (let [board (position-state->board position-state)
        budget-ms (compute-time-ms go-opts position-state)
        deadline (+ (System/currentTimeMillis) budget-ms)
        net @network
        move (az/best-uci-move board net {:deadline deadline
                                          :stop? #(deref stop-search)
                                          :simulations default-mcts-sims})]
    (println (str "bestmove " (or move "(none)")))))

;; --- UCI loop ---------------------------------------------------------------

(defn handle-uci-command
  [input position-state]
  (let [line (str/trim input)
        lower (str/lower-case line)]
    (cond
      (= lower "uci")
      (do
        (println (str "id name " engine-name))
        (println (str "id author " engine-author))
        (println "uciok")
        position-state)

      (= lower "isready")
      (do
        (println "readyok")
        position-state)

      (= lower "ucinewgame")
      {:kind :startpos :moves []}

      (str/starts-with? lower "position ")
      (or (parse-position-line line) position-state)

      (str/starts-with? lower "go")
      (do
        (run-search position-state (parse-go line))
        position-state)

      (= lower "stop")
      (do (reset! stop-search true) position-state)

      (= lower "quit")
      (do (reset! stop-search true) ::quit)

      :else position-state)))

(defn uci
  []
  (start-input-reader)
  (loop [position-state {:kind :startpos :moves []}]
    (let [input (.take command-queue)
          next-state (handle-uci-command input position-state)]
      (when-not (= ::quit next-state)
        (recur next-state)))))

(defn -main
  [& args]
  (case (first args)
    "train" (apply train/-main (rest args))
    (do
      (native/print-backend-status!)
      (load-network!)
      (uci))))
