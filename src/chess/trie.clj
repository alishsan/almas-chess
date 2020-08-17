
(ns chess.trie
(require [clojure.string :as str])
 )

(defn add-to-trie [trie [move score]]
  (assoc-in trie move (merge (get-in trie move) {:score score})))

(defn score [trie x]
  "Returns the score if the node exists in the specified trie."
  (:score (get-in trie x) ))

(defn getnode [trie node]
"Return the position and score"
[node (score trie node)]
)

(defn nodes-below [trie node]
  (map (partial conj node) (keys (dissoc (get-in trie node) :score)))
)

(defn moves-below [trie node]
   (keys (dissoc (get-in trie node) :score))
)

(defn scores-below [trie node]
  (map (partial score trie)  (nodes-below trie node )))

(defn moves-scores-below [trie node] 
  (into {}  (for [move (moves-below trie node)] [move (((trie node) move) :score)])))

(defn build-trie [coll]
  "Builds a trie over the values in the specified seq coll."
  (reduce add-to-trie {} coll))

(defn node-above
[position]
  (into [] (drop-last position)))

(defn update-trie 
"Updates, or backpropagates a trie"
  [trie0 node]

  (let [position0 node eval (first (score trie0 node)) nsim (second (score trie0 node))]
    (loop [trie trie0 position position0]
      (let [values (score trie position) up-position (node-above position)  up-values ( score trie up-position)]
       
        (if (not (first up-values))
          trie  
          (recur (add-to-trie trie [up-position [(+ eval  (first up-values) ) (+ nsim (second up-values))]] )
 up-position)
          )))))

;(def coll [[["e2e4"] [nil 0]] [["e2e4" "d7d5"] [1 0]]] )

;(def trie (build-trie (map coll)))

