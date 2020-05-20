(ns chess.core

(require [clojure.set :as set])
(require [clojure.string :as str])
  (:gen-class))

(use 'chess.basics)


(defn nodes-below
[tree position]
(let [cp (count position)]
(into {}  (filter #(.contains (first %) position) 
                 (filter #(and (< (count (first %)) (+ cp 7))  (> (count (first %)) cp) ) tree))
)))

(defn add-to-tree
"add a position to a tree, used in expansion"
[tree position]
   (conj (into {} position) tree)
)

(defn ratio
[values]
 (/ (first values) (second values))
)

(defn ucb1+ 
"upper limit"
[values total]
  (+ (/ (first values) (second values)) (Math/sqrt (/ (* 2 (Math/log total)) 
                                                      (second values))))
)

(defn ucb1-
"lower limit"
[values total]
  (- (/ (first values) (second values)) (Math/sqrt (/ (* 2 (Math/log total)) 
                                                      (second values))))
)

(defn best-move
[tree position]
  (let [total (second (tree position)) 
nodes (nodes-below tree position) 
plies (count (str/split position #" "))
        pos (first (if (odd? plies) (apply max-key (comp #(ucb1- % total) val) nodes)
 (apply min-key (comp #(ucb1+ % total) val) nodes)) 
) ]
(last (str/split pos #" "))
))


(defn ordered-moves
[tree position]
  (let [total (second (tree position)) 
nodes (nodes-below tree position) 
plies (count (str/split position #" "))
        pos (first (if (odd? plies) (apply sort-by (comp #(ucb1- % total) val) nodes)
 (apply sort-by (comp #(ucb1+ % total) val) nodes)) 
) ]
(last (str/split pos #" "))
))

(defn select
[tree topnode]
(let [total (second(tree topnode))]
(loop [node topnode] (let [nodes (nodes-below tree node) plies (count (str/split node #" "))]
(if (or (empty? nodes) (some nil? (first (vals nodes))))
[tree [node (tree node)]]
(recur (first (cond (odd? plies) (apply max-key (comp #( ucb1+ % total) val) nodes) 
:else (apply min-key (comp #(ucb1- % total) val) nodes) 
)))))
)))

;(cond (= stm \w) (ucb1 % total))


(defn expand
[tree]
  (let [node (rand-nth (keys tree)) board  (set-board node)]

  (println node)
  ;(if (first) (first) (vals tree))
  (add-to-tree tree
               (for [move (valid-moves board)] 
                 {(str node " " (to-algebra  move))  [nil 0]}))
                                        ;tree ;don't expand if not simulated
      
  ))

(defn expandall
"expand to include all-the possible moves"
[tree node]

(let [board  (set-board  (first node))]
(if (first (first (vals tree)))
 (add-to-tree tree
(for [move (valid-moves board)] 
   [(str (first node) " " (to-algebra  move))  [nil 0]]))
tree ;don't expand if not simulated
)
))

(defn simulate-one
  [board] ; like ["startpos" [nil 0]]
(if (first (second board))
board

  (loop [position (set-board (first board)) move-count 1]
    (let [moves (valid-moves2 position) gresult (game-result position) ] 
      (if gresult [(first board) [(* gresult 50) 50]]
          (let [ move (rand-nth moves) nextpos   (move-piece position (first move) (second move)) result (or (game-result nextpos) (if (> move-count 100) 1/2)) ]
            (if result  
              [(first  board) [result 1]]  

              (recur nextpos (inc move-count))
              )))))))

(defn simulate
  [tree] ; like ["startpos" [nil 0]
  (let [niltree (filter #(nil? (first (second %))) tree)]
    (reduce conj tree (map simulate-one niltree)))) 
  

(defn update-tree
"Updates, or backpropagates a tree"
  [tree0 position0]
  (let [eval (first (tree0 position0)) nsim (second (tree0 position0))]
    (loop [tree tree0 position position0]
      (let [values (tree position) up-position (str/join " " (drop-last (str/split position #" "))) up-values (tree up-position)]
       
        (printfv position up-position (+ (first values)  (first up-values) )) ;debug
        (if (not (first up-values))
          tree  
          (recur (conj tree [up-position [(+ eval  (first up-values) ) (+ nsim (second up-values))]] ) up-position)
          )))))


(defn update-tree2
"Updates, or backpropagates a tree"
[tree node]
  (let [nodes (keys (nodes-below tree node))]

    (if (empty? nodes) tree

   ;     (reduce #(add-nodes-below %1  %2) tree nodes)
     (reduce update-tree tree nodes)
)
))


(defn add-nodes-below "adds the values of the nodes below to the current node"
[tree node]
 (let [nodes (conj (keys (nodes-below tree node)) node)]
    (if (empty? nodes) tree
(conj tree [node (apply map + (map tree nodes))])
)))

(defn mcts "Monte Carlo Tree Search"
[board]
(loop [tree {board [nil 0]}]
  (let [positions (keys (filter #(nil? (first (second %))) tree))] ;(first (filter #(not (second %)) tree))
;(println posval)
(if (> (count tree) 50)
  (into {} (filter #(first (second %)) tree))
(recur (apply expandall
       (select (reduce update-tree (simulate tree) positions) board)))
))))  

(defn uci
[]
(def nameEngine "almas")
(println "name" nameEngine "\n")
(loop [input (str/lower-case (read-line)) position "startpos"]
(when (not= input "quit")
(let [input-vector (rest (str/split input #" "))] ;remove the word "position"
(if (= "uci" input)
(println "id" nameEngine " \n uciok \n"))

(if (= "isready" input)
(println "readyok \n"))

(if (str/includes? input "go")
(println   "bestmove" (best-move  (mcts position) position))) 
(recur (str/lower-case (read-line)) (str/join " " (remove #(= "moves" %) input-vector)))
)
)))

(defn -main
[]
(uci)
)
