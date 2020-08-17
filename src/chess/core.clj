;(set! *warn-on-reflection* true)

(ns chess.core

(require [clojure.set :as set])
(require [clojure.string :as str])
  (:gen-class))

;(require '[me.raynes.conch :refer [programs with-programs let-programs] :as sh])
(require '[me.raynes.conch.low-level :as sh])
(require '[criterium.core :as criterium])
(use 'chess.basics)
(use 'chess.trie)
 (use 'criterium.core)


(def stf (sh/proc "stockfish"))
;(sh/read-line stf :out)
;(sh/feed-from-string stf "position startpos\n")
;(sh/feed-from-string stf "validmoves\n")
;(str/split (sh/read-line stf :out) #" ")

(defn validmoves 
[board]
  (let [pos (str/replace (str/join " " board) #"startpos" "position startpos moves" )]
    (sh/feed-from-string stf (str pos "\n"))
(sh/feed-from-string stf "validmoves\n")
(str/split (sh/read-line stf :out) #" ")
))

(defn validmove 
[board]
  (let [pos (str/replace (str/join " " board) #"startpos" "position startpos moves" )]
    (sh/feed-from-string stf (str pos "\n"))
(sh/feed-from-string stf "validmove\n")
(sh/read-line stf :out)
))



(defn simulate-stf; calls stockfish to simulate a position
[position]
  (let  [pos (str/replace position #"startpos" "position startpos moves" )]
;(println "sims" pos)
    (sh/feed-from-string stf (str pos "\n"))
    (sh/feed-from-string stf "simulate\n")
    (let [sim (read-string (sh/read-line stf :out))] 
      (if sim sim (recur position)))))

(defn simulateone
[node score]
  (let  [pos (str/join " " node)  ]
;(println "sims" pos)
    (let [sim (simulate-stf pos)]
      [node [(+ (or (first score) 0) sim) (inc (second score))]]

)))


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
  (- (/ (first values) (second values)) (Math/sqrt (/ (* 2  (Math/log total)) 
                                                      (second values))))
)

(defn plies [position] 
  (+ (count (rest position)) (count (str/split (first position) #" ")))
)


(defn best-move
[tree position]
  (let [total (second (score tree position)) 
        nodes (filter  #(some? (map first (map (partial score tree) %))) (nodes-below tree position))] 
    (last (if (odd? (plies position)) (apply max-key (comp #(ucb1- % total)  (partial score tree)) nodes)
              (apply min-key (comp #(ucb1+ % total)  (partial score tree)) nodes))) 
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
(let [total (second ( score tree topnode))]
  (loop [node topnode] (let [nodes (into [node]  
                                         (nodes-below tree node))  plies (plies node)] ; NB assuming notation is always from the starting position, not fen

(if (or (empty? nodes) (some nil? (map first (map (partial score tree) nodes))))
  [tree node ]
(let [selected-node  (if (odd? plies) (apply max-key (comp #( ucb1+ % total)  (partial score tree)) nodes) 
                           (apply min-key (comp #(ucb1- % total)  (partial score tree)) nodes))]

  (if (=  selected-node node)
    [tree node]
(recur selected-node)
))
))
)))

;(cond (= stm \w) (ucb1 % total))



(defn expand
[tree node]
  (let [board (first node) newnode  [(str board " " (validmove board))  [nil 0]]]
    (if  (first (tree (first newnode)))
[tree node]        ;don't expand 
      [(add-to-trie tree newnode) newnode])
    
                                
      
  ))



(defn expandall
"expand to include all-the possible moves"
[trie node]

  (if (not-empty  (nodes-below trie node)) ; if already expanded
    [trie node]                       ; just return the input, else
    (let [board node vm (validmoves board)]

      (if (and  (first (score trie node)) (not-empty (first vm))) ;if the node has been simulated and there are valid moves
        [(reduce add-to-trie trie
                 (for [move vm] 
                   [(conj node  move)  [nil 0]])) node]
        [trie node])                         ;don't expand if not simulated
      
      )))



(defn simulate
  [trie node0] ; node is like ["startpos" "e2e4"]

  (let [node (if (first (score trie node0)) ; if already simulated
                (or (first (filter #(nil? (first (score trie %))) (nodes-below trie node0))) node0) ; use one of the nodes below to simulate, or the node again if all simulated
                node0                             ; if not, use node0
                )]

    (let [newnode (simulateone node (score trie node))]
;      (println "simul "  newnode)
      [(add-to-trie trie newnode) (first newnode)]
))) 
  



(defn update-trie2
"Updates, or backpropagates a trie"
[trie node]
  (let [nodes (keys (nodes-below trie (first node)))]

    (if (empty? nodes) trie

     (reduce update-trie trie nodes)
)
))




  
(defn mcts "Monte Carlo Tree Search using modified stockfish"
[board] ;board is like "startpos e2e4"
;(sh/read-line stf :out)
 
;  (let [board (str/replace board0 #"startpos moves" "startpos" )])
  (let [vm (validmoves [board]) trienode0  (simulate {board {:score [nil 0]}} [board])]
    (if (= 1 (count vm)) ;if there is only one move, just add it and return the tree, simulating only once
      (add-to-trie (first trienode0) [[board (first vm)] (score (first trienode0) [board])]) ; 

      (loop [trienode (simulate {board {:score [nil 0]}} [board])] ; trienode -- trie and node
        (let [total  (second (score (first trienode) [board]))]
                                        ;(println "total " total)
          (if (> total 100000)
            (first trienode)
            (recur  (apply simulate (apply expandall
                                           (select (apply update-trie trienode) [board]))))
            )))))) 


(defn uci
[]
(def nameEngine "almas")
;(sh/read-line stf :out)

(println "name" nameEngine "\n")
(loop [input (str/lower-case (read-line)) position "startpos"]
(when (not= input "quit")
(let [input-vector (rest (str/split input #" "))] ;remove the word "position"
(if (= "uci" input)
(println "id" nameEngine " \n uciok \n"))

(if (= "isready" input)
(println "readyok \n"))

(if (str/includes? input "go")
  (println   "bestmove" (best-move  (mcts position) [position]))) 
(recur (str/lower-case (read-line)) (str/join " " (remove #(= "moves" %) input-vector)))
)
)))

(defn -main
[]
;(sh/read-line stf :out)
(uci)
)
