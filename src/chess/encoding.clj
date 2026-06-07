(ns chess.encoding
  "AlphaZero-style board planes flattened to a feature vector."
  (:require [chess.basics :as b]))

(def ^:const plane-count 13)
(def ^:const board-size 64)
(def ^:const feature-size (* plane-count board-size))

(def ^:private piece->plane
  {\P 0 \N 1 \B 2 \R 3 \Q 4 \K 5
   \p 6 \n 7 \b 8 \r 9 \q 10 \k 11})

(defn- plane-offset
  [plane sq]
  (+ (* plane board-size) sq))

(defn board->features
  "Return a float vector of length feature-size for the current player to move."
  [board]
  (let [features (float-array feature-size)
        stm (:side-to-move (board 64))]
    (doseq [sq (range board-size)
            :let [piece (:piece (board sq))]]
      (when-let [plane (piece->plane piece)]
        (aset features (plane-offset plane sq) 1.0)))
    (doseq [sq (range board-size)]
      (when (= stm "w")
        (aset features (plane-offset 12 sq) 1.0)))
    features))

(defn terminal-value
  "Game outcome from white's perspective in [-1, 1], or nil if non-terminal."
  [board]
  (let [result (b/game-result board)]
    (cond
      (= result 1) 1.0
      (= result 0) -1.0
      (= result 1/2) 0.0
      :else nil)))

(defn perspective-value
  "Flip terminal value to the side-to-move perspective."
  [board value]
  (if (= (:side-to-move (board 64)) "w")
    value
    (- value)))
