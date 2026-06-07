(ns chess.action
  "Fixed action space: from-square * 64 + to-square (4096 actions)."
  (:require [chess.basics :as b]
            [chess.native :as native]
            [clojure.string :as str]))

(def ^:const action-size (* 64 64))

(def ^:private alpha-start (int \a))

(defn- square
  [sq]
  (mod sq 64))

(defn- file-rank->sq
  [^String s]
  (+ (- (int (.charAt s 0)) alpha-start)
     (* 8 (- (Integer/parseInt (subs s 1 2)) 1))))

(defn uci->index
  [uci]
  (let [u (str/trim uci)]
    (+ (file-rank->sq (subs u 0 2))
       (* 64 (file-rank->sq (subs u 2 4))))))

(defn move->index
  "Encode an internal [from to] move into [0, action-size)."
  [[from to]]
  (+ (square from) (* 64 (square to))))

(defn index->squares
  [idx]
  [(mod idx 64) (quot idx 64)])

(defn- uci->move-pair
  [uci]
  (let [u (str/trim uci)
        from (file-rank->sq (subs u 0 2))
        to-base (file-rank->sq (subs u 2 4))
        promo? (and (> (count u) 4) (Character/isLetter (.charAt u 4)))
        to (if promo? (+ 100 to-base) to-base)]
    [from to]))

(defn- clojure-legal-moves
  [board]
  (mapv move->index (b/valid-moves board)))

(defn legal-move-indices
  [board]
  (if-let [ucis (seq (native/legal-uci-moves board))]
    (mapv uci->index ucis)
    (clojure-legal-moves board)))

(defn index->move
  "Decode an action index to a legal move on board, or nil."
  [board idx]
  (or (when (native/enabled?)
        (some (fn [uci]
                (when (= idx (uci->index uci))
                  (uci->move-pair uci)))
              (native/legal-uci-moves board)))
      (let [[from to-sq] (index->squares idx)]
        (first (for [mv (b/valid-moves board)
                     :when (and (= from (first mv))
                                (= to-sq (square (second mv))))]
                 mv)))))

(defn uci->move
  [board uci]
  (if (native/enabled?)
    (uci->move-pair uci)
    (first (for [mv (b/valid-moves board)
                 :when (= uci (b/to-algebra mv))]
             mv))))

(defn move->uci
  [move]
  (b/to-algebra move))
