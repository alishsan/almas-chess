(ns chess.native
  "Legal move generation via patched Stockfish (validmoves command).
   Falls back to Clojure when Stockfish is unavailable.
   Set ALMAS_STOCKFISH to the binary path; disable with ALMAS_NATIVE=false."
  (:require [chess.basics :as b]
            [clojure.string :as str])
  (:import [java.io BufferedReader OutputStreamWriter InputStreamReader]))

(def ^:private default-vendor-binary
  (let [root (System/getProperty "user.dir")
        bin (java.io.File. root "vendor/stockfish/src/stockfish")]
    (when (.canExecute bin)
      (.getAbsolutePath bin))))

(def ^:private stockfish-path
  (or (System/getenv "ALMAS_STOCKFISH")
      default-vendor-binary
      "stockfish"))

(def ^:private native-disabled?
  (= "false" (System/getenv "ALMAS_NATIVE")))

(defonce ^:private enabled-state (atom nil))

(def ^:ThreadLocal ^:private proc-local (ThreadLocal.))

(defn board->fen
  [board]
  (b/board-to-fen board))

(defn- read-until!
  [^BufferedReader in pred]
  (loop []
    (when-let [line (.readLine in)]
      (if (pred line)
        line
        (recur)))))

(defn- write!
  [^OutputStreamWriter out s]
  (.write out (str s "\n"))
  (.flush out))

(defn- read-validmoves-line!
  [^BufferedReader in]
  (loop []
    (when-let [line (.readLine in)]
      (if (or (str/blank? line)
              (str/starts-with? line "info ")
              (= line "readyok"))
        (recur)
        line))))

(defn- start-process! []
  (let [^Process p (.start (ProcessBuilder. [stockfish-path]))
        in (BufferedReader. (InputStreamReader. (.getInputStream p)))
        out (OutputStreamWriter. (.getOutputStream p))]
    (write! out "uci")
    (read-until! in #(= % "uciok"))
    (write! out "isready")
    (read-until! in #(= % "readyok"))
    {:process p :in in :out out}))

(defn- process! []
  (if-let [p (.get proc-local)]
    p
    (let [p (start-process!)]
      (.set proc-local p)
      p)))

(defn try-enable!
  "Probe Stockfish; return true if validmoves works on startpos."
  []
  (if native-disabled?
    false
    (try
      (let [{:keys [^Process process in out]} (start-process!)]
        (write! out "position startpos")
        (write! out "validmoves")
        (let [line (read-validmoves-line! in)
              ok (and line (pos? (count (str/split (str/trim line) #"\s+"))))]
          (.destroy process)
          ok))
      (catch Exception e
        (binding [*out* *err*]
          (println "Native backend unavailable:" (.getMessage e)))
        false))))

(defn- ensure-probed! []
  (when (= @enabled-state nil)
    (reset! enabled-state (try-enable!))))

(defn enabled? []
  (when-not native-disabled?
    (ensure-probed!))
  (true? @enabled-state))

(defn legal-uci-moves
  [board]
  (when (enabled?)
    (let [{:keys [out in]} (process!)]
      (write! out (str "position fen " (board->fen board)))
      (write! out "isready")
      (read-until! in #(= % "readyok"))
      (write! out "validmoves")
      (when-let [line (read-validmoves-line! in)]
        (->> (str/split (str/trim line) #"\s+")
             (remove str/blank?)
             vec)))))

(defn print-backend-status!
  []
  (if (enabled?)
    (println "Move generation: Stockfish native (" stockfish-path ")")
    (do
      (println "Move generation: Clojure (SLOW — build ./scripts/build-stockfish.sh)")
      (when (and (not (System/getenv "ALMAS_STOCKFISH"))
                 (not default-vendor-binary))
        (println "  hint: run ./scripts/build-stockfish.sh or set ALMAS_STOCKFISH")))))

(defn shutdown! []
  (when-let [{:keys [^Process process]} (.get proc-local)]
    (try
      (.destroy process)
      (catch Exception _))
    (.remove proc-local)))
