;(set! *warn-on-reflection* true)

(ns chess.basics

(require [clojure.set :as set])
(require [clojure.string :as str])

 )


(declare game-over)
;for debugging
(def ^:dynamic *verbose* true)

(defmacro printfv
  [fmt & args]
  `(when *verbose*
     (printf ~fmt ~@args)))

(defmacro with-verbose
  [& body]
  `(binding [*verbose* true] ~@body))
;end debugging code


(defmacro loop-it
[bindings body finish]
{:pre [(vector? bindings) (even? (count bindings))
       (vector? (last bindings)) (= :let (last (butlast bindings)))]}
(let [it-binds (butlast (butlast bindings))
      sym-binds (take-nth 2 it-binds)
      colls (take-nth 2 (rest it-binds))
      iter-syms (repeatedly (count sym-binds) #(gensym "iter"))]
  `(let ~(vec (mapcat (fn [it coll]
                        [it `(clojure.lang.RT/iter ~coll)])
                      iter-syms colls))
     (loop ~(vec (last bindings))
       (if (and ~@(map (fn [it] `(.hasNext ~it)) iter-syms))
         (let ~(vec (mapcat (fn [bind it]
                              [bind `(.next ~it)])
                            sym-binds iter-syms))
           ~body)
         ~finish)))))

(defn isoff-board 
"Checks if a square is offboard"
[n]
(< 0 (bit-and (+ n (* 8 (quot n 8))) 0x88))
)


(def ^String piece-string "PNBRQKpnbrqk")


(def empty-board
(loop [brd [{}] cnt 1]
(if (> cnt 63)
brd
(recur (conj brd {}) (inc cnt))
)))

;; Starting position
(def start-board 
[{:pos 0, :piece \R} {:pos 1, :piece \N} 
{:pos 2, :piece \B} {:pos 3, :piece \Q} 
{:pos 4, :piece \K} {:pos 5, :piece \B} 
{:pos 6, :piece \N} {:pos 7, :piece \R} 
{:pos 8, :piece \P} {:pos 9, :piece \P} 
{:pos 10, :piece \P} {:pos 11, :piece \P} 
{:pos 12, :piece \P} {:pos 13, :piece \P} 
{:pos 14, :piece \P} {:pos 15, :piece \P} 
{:pos 16, :piece \-} {:pos 17, :piece \-} 
{:pos 18, :piece \-} {:pos 19, :piece \-} 
{:pos 20, :piece \-} {:pos 21, :piece \-}
{:pos 22, :piece \-} {:pos 23, :piece \-} 
{:pos 24, :piece \-} {:pos 25, :piece \-} 
{:pos 26, :piece \-} {:pos 27, :piece \-}
{:pos 28, :piece \-} {:pos 29, :piece \-} 
{:pos 30, :piece \-} {:pos 31, :piece \-}
{:pos 32, :piece \-} {:pos 33, :piece \-} 
{:pos 34, :piece \-} {:pos 35, :piece \-} 
{:pos 36, :piece \-} {:pos 37, :piece \-} 
{:pos 38, :piece \-} {:pos 39, :piece \-} 
{:pos 40, :piece \-} {:pos 41, :piece \-}
{:pos 42, :piece \-} {:pos 43, :piece \-}
{:pos 44, :piece \-} {:pos 45, :piece \-} 
{:pos 46, :piece \-} {:pos 47, :piece \-}
{:pos 48, :piece \p} {:pos 49, :piece \p} 
{:pos 50, :piece \p} {:pos 51, :piece \p} 
{:pos 52, :piece \p} {:pos 53, :piece \p} 
{:pos 54, :piece \p} {:pos 55, :piece \p} 
{:pos 56, :piece \r} {:pos 57, :piece \n} 
{:pos 58, :piece \b} {:pos 59, :piece \q} 
{:pos 60, :piece \k} {:pos 61, :piece \b}
 {:pos 62, :piece \n} {:pos 63, :piece \r}
{ :side-to-move "w", :castles "KQkq",  :ep-capture "-", :fifty-move-counter 0, :move-number 1}
]
)

(def start-fen  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")


(defn black-total-pv ;total piece values for black
[board]
  (let [piece-values {\p 1, \n 3, \b 3, \r 5, \q 9}]
    (apply + (filter identity (map piece-values (map :piece (drop-last board))))))
)

(defn white-total-pv ;total piece values for white
[board]
  (let [piece-values {\P 1, \N 3, \B 3, \R 5, \Q 9}]
    (apply + (filter identity (map piece-values (map :piece (drop-last board))))))
)

(defn total-pv ;sum of the values all the pieces (except kings)
[board]
  (let [piece-values {\p 1, \n 3, \b 3, \r 5, \q 9 \P 1, \N 3, \B 3, \R 5, \Q 9}]
    (apply + (filter identity (map piece-values (map :piece (drop-last board))))))
) 


(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)

(def ansi-styles                                                             
  {:red   "[31m"                                                             
   :green "[32m"                                                             
   :blue  "[34m"                                                             
   :reset "[0m"})                                                            
                                                                             
(defn ansi                                                                   
  "Produce a string which will apply an ansi style"                          
  [style]                                                                    
  (str \u001b (style ansi-styles)))                                          
                                                                             
(defn colorize                                                               
  "Apply ansi color to text"                                                 
  [text color]                                                               
  (str (ansi color) text (ansi :reset)))                                     
                                                                             
           
(defn render-pos
  [board sq]
(let [ch (get-in board [sq :piece])]
  (if (not (= ch \-))
       (if (Character/isLowerCase ch)
         (colorize ch :blue)                            
         (if (Character/isUpperCase ch)
           (colorize ch :red)))
  (colorize "-" :reset))            
 ))

(defn row-positions
  "Return all positions in the given row"
  [row-num]
(let [addnum (* 8 row-num)]
  (range (+ addnum 0) (+ addnum 8) ))
)
(defn render-row
  [board row-num]
  (str (str/join " " (map (partial render-pos board) 
 (row-positions row-num)))))
         
(defn print-board
  [board]
  (doseq [row-num (range 7 -1 -1)] ;; print starting from last rank (black on top)
    (println (render-row board row-num))))

(defn isempty?
  "Does the position have a piece in it?"
  [board pos]
(if (< -1 pos)
  (= (:piece (board (rem pos 64))) \-))
)

(defn iswhite?
  "Does the position have a piece in it?"
  [board pos]
(if (or (> pos  63) (< pos 0))
false
(let [ch  (:piece (board pos))]
(if ch
  (Character/isUpperCase ch)
false)
)))

(defn isblack?
  "Does the position have a piece in it?"
  [board pos]
(if (or (> pos  63) (< pos 0))
false
(let [ch  (:piece (board pos))]
(if ch
  (Character/isLowerCase ^char ch)
false)
)))




(defn remove-piece
  "Take the piece at given position out of the board"
  [board sq]
;  (assoc-in board [sq :piece] \-)
  (assoc board sq {:pos sq :piece \-})
)

(defn place-piece
  "Put a piece (character) in the board at given position"
  [board square piece]
(if (or (> square 63) (< square 0))
(println "warning: off-board move" square piece)
)
(if (.contains piece-string (str piece))
  (assoc board square  { :pos square :piece  piece })
(remove-piece board square);(println "Wrong piece!")
))         


(defn move-piece
  "Take piece out of p1 and place it in p2"
  [board p1 p2old]
  (let [piece (:piece (board p1)) p2 (if (and (> p2old 99) (= (Character/toUpperCase piece) \P)) (- p2old 100) p2old) ;promotions
 brd (place-piece (remove-piece board p1) p2 ((board p1) :piece)) stm (:side-to-move (brd 64)) board1 (assoc-in brd [64 :side-to-move] (if (= stm "w") "b" "w"))]
;(if (and (> p1 47) (> p2 55) (= piece \P)) (println "warning: off-board move" p1 p2 piece))
(if (= (Character/toLowerCase piece) \k) ;castling
(if (= p1 (- p2 2)) 
 (place-piece (remove-piece board1 (inc p2)) (dec p2) ((board (inc p2)) :piece))
(if (= p1 (+ p2 2)) 
 (place-piece (remove-piece board1 (- p2 2)) (inc p2) ((board (- p2 2)) :piece))
(assoc-in board1 [64 :castles] "-"))
)
(cond
 (and (= (quot p2 8) 7) (= \P piece)) (assoc-in board1 [p2 :piece] \Q)
  (and (= (quot p2 8) 0) (= \p piece)) (assoc-in board1 [p2 :piece] \q)
:else board1))
))

(defn fen-to-square 
"given fen starting from a square, fills the row starting from that square"
[s board sq]
(let [ch (first s)]
(if (> (count s) 0)
(if (> (Character/digit ch 10) 1)
(recur (apply str (dec (Character/digit ch 10)) (rest s)) (remove-piece board sq)  (inc sq))
(recur (apply str (rest s)) (place-piece board sq ch) (inc sq)))
board)))

(defn fen-to-board
[s]
(let [svec (str/split s #" ") rows (str/split (first svec) #"/")]
(loop [rank 7 rboard empty-board]
(if (< rank 0)
  (assoc rboard 64 { :side-to-move (second svec) , :castles (svec 2),  :ep-capture (svec 3), :fifty-move-counter (read-string (svec 4)), :move-number  (read-string (svec 5))})
(recur (dec rank) (fen-to-square (rows rank) rboard (* (- 7 rank) 8)))
))
))


(defn empty-to-num 
"represents string of empty squares by a number, 'rq---PR-' ->  'rq3PR1' "
[strng]
(reduce (fn [x y]  
(let [qwsd (Character/digit (or (last x) \-) 10)]
(str  (if (and (= y \-) (< 0 qwsd)) (apply str (drop-last x)) x)  (or (if (= y \-) (if (< 0 qwsd) (inc qwsd) \1) y)))))
 "" strng)
)


(defn board-to-fen 
"converts a board into a fen string (Forsyth-Edwards Notation)"
[board]
(loop [str1  (take 64 (map :piece board)) result [] ]
(if (< (count str1) 8)
(let [extra (map last (get board 64)) ch (clojure.string/join " "  extra)]
(str (clojure.string/join "/" result) " " ch)) 
(recur (drop-last 8 str1) (conj result ( empty-to-num (apply str (take-last 8 str1)))))
)))

(defn move-from-algebra
"changes from algebraic notation to square numbers"
[text]

[(+ (- (int (first text)) alpha-start)
    (* 8 (- (Character/digit (second text) 10) 1)))
( + (- (int (second (rest text))) alpha-start)
(* 8 (- (Character/digit (last (take 4 text)) 10) 1)) )]
)


(defn to-algebra 
  "takes a pos-to-pos move and turns into algebraic notation (so far w/o promotion)"
  [move]
  (let [text (reduce (fn [cum y] (let [x (rem y 100) rank  (quot x 8) ] (str cum (char (+ (rem x 8) alpha-start)) (inc rank)))) "" move) p1 (first move) p2 (second move)]
    (if (or (> p2 99)) 
      (str text "q")
      text
      ) ))
 
(defn make-move 
"function used in set-position"
[board x]
 ; (if (empty? x) board)
(let [y (move-from-algebra x)
board1 (move-piece board (first y) (second y) )] 
(if (= 4 (count x))
board1
 (place-piece board1 (second y) (if (< (second y) 8) (last x) (Character/toUpperCase (last x))) )
)
))

(defn set-board
"sets position for MCTS"
[input]
(if (= (count input) 8) start-board; if starting position
(let [board start-board input-vector (str/split input #" ")]
(reduce make-move board (rest input-vector))

))
)


(defn set-position
"sets position for uci"
[^String input position]

(if (.contains input "startpos")
(let [board start-board input-vector (str/split input #" ")]

(if (some #(= "moves" %) input-vector)
(loop [tvector input-vector]
(if (not (some #(= "moves" %) tvector))
(reduce make-move board tvector)
(recur (rest tvector))
))
(if  (.contains input "fen")
(let [fen (apply str (drop 13 input))]
 (fen-to-board fen)
)
position
)
) board )))


(defn knight-moves
"find and return the list of knight moves"
[pos]
(let [rank (quot pos 8) file (rem pos 8) xy (map list [1 2 1 2 -1 -2 -1 -2] [2 1 -2 -1  2 1 -2 -1])] 
(reduce (fn [cum var] (let [posx (+ file (first var))  posy  (+ rank (second var))]
(if (and (> posx -1) (> posy -1)  (< posx 8) (< posy 8))
(conj cum (list pos (+ posx (* posy 8))))
cum) 
)) [] xy)))

(defn find-knight-moves
"find and return the list of knight moves"
[board]
(let [stm (:side-to-move (board 64)) knights  (map :pos (filter #(= (:piece %) (if (= stm "w") \N \n)) board))]

(filter #( not ((if (= stm "w") iswhite? isblack?) board (second %)))
(reduce into [] (map  knight-moves knights)))
))

(defn pawn-moves
"finds pawn moves from a position and side to move (stm)"
[pos stm] 
(if (> pos 55) (println "hey! how come a pawn is 8 rank" pos stm))
  (let [rank (quot pos 8) factor (if (= stm "w") 1 -1)
result (if ( or (= rank factor) (= rank (+ factor 7)))
  [ (list pos (+ pos (* factor 8))) (list pos (+ pos  (* factor 16)))]
[(list pos (+ pos  (* factor 8)))]
)]

(for [x result]
(cond 
(> (second x) 55)  (list (first x) (+ 100 (second x)))  
(< (second x) 8)  (list (first x) (+ 100 (second x) )) 
:else x
))
))

(defn pawn-captures
"Given position and side move find captures"
[pos stm]
 (let [file  (rem pos 8) rank (quot pos 8) left (- file 1) right (+ file 1) factor (if (= stm "w") 1 -1)
result (for [x [left right]
:when (and (>= x 0) (< x 8))]
(list pos (+ x (* 8 (+ factor rank))))
)]
(for [x result]
(cond 
(> (second x) 55) (list (first x) (+ 100 (second x))) 
(< (second x) 8) (list (first x) (+ 100 (second x) )) 
:else x
))))

(defn find-pawn-moves
"find and return the list of pawn moves"
[board]
  (let [stm (:side-to-move (board 64)) pawns  (map :pos (filter #(= (:piece %) (if (= stm "w") \P \p)) board)) ]
  (into 
   (filter #(and (isempty? board  ((if (= stm "w") + -) (first %) 8)) (isempty? board (rem (second %) 100)))
(reduce #(into %1 (pawn-moves %2 stm)) [] pawns))
   (filter #((if (= stm "b") iswhite? isblack?) board ( rem (second %) 100))
(reduce #(into %1 (pawn-captures %2 stm)) [] pawns))
)
))

(defn rook-moves
[board pos]
(let [stm (:side-to-move (board 64)) isopposite?  (if (= stm "w") isblack? iswhite?) issame-color?  (if (= stm "w") iswhite?  isblack?)]
(reduce into (loop [x pos cum []] ; search up until hit a piece 
 (if  (or (issame-color? board (+ x 8)) (>  x 55) (isopposite? board x))
 cum 
(recur (+ x 8) (into cum [(list pos (+ x 8))]))))

 [(loop [x pos cum []] (if  (or  (< x 8) (issame-color? board (- x 8))  (isopposite? board x))
 cum 
(recur (- x 8) (into cum [(list pos (- x 8))]))))

 (loop [x pos cum []] (if  (or  (< (rem x 8) 1) (issame-color? board (- x 1))  (isopposite? board x)) cum 
(recur (- x 1) (into cum [(list pos (- x 1))]))))
 (loop [x pos cum []] (if  (or  (> (rem x 8) 6) (issame-color? board (+ x 1))  (isopposite? board x)) cum 
(recur (+ x 1) (into cum [(list pos (+ x 1))]))))]
)))

(defn find-rook-moves
[board]
(let [stm (:side-to-move (board 64)) rooks (map :pos (filter #(= (:piece %) (if (= stm "w") \R \r)) board))]
(reduce into [] (map #(rook-moves board %) rooks))
 ))

(defn bishop-moves
"Find the bishop moves from a square"
[board pos] 
(let [stm (:side-to-move (board 64)) isopposite?  (if (= stm "w") isblack? iswhite?) issame-color?  (if (= stm "w") iswhite?  isblack?)]
(reduce into (loop [x pos cum []] ; search up-right until hit a piece 
 (if  (or (issame-color? board (+ x 9)) (>  x 55) (isopposite? board x) (= (rem x 8) 7))
 cum 
(recur (+ x 9) (into cum [(list pos (+ x 9))]))))

 [(loop [x pos cum []] (if  (or  (< x 8) (issame-color? board (- x 7))  (isopposite? board x) (= (rem x 8) 7)); search down-right 
 cum 
(recur (- x 7) (into cum [(list pos (- x 7))]))))

 (loop [x pos cum []] (if  (or  (< (rem x 8) 1) (issame-color? board (- x 9))  (<  x 8) (isopposite? board x)) cum  ; down-left
(recur (- x 9) (into cum [(list pos (- x 9))]))))
 (loop [x pos cum []] (if  (or  (< (rem x 8) 1)  (>  x 55) (issame-color? board (+ x 7))  (isopposite? board x)) cum ; up-left 
(recur (+ x 7) (into cum [(list pos (+ x 7))]))))]
)
))

(defn find-bishop-moves
[board]
(let [stm (:side-to-move (board 64)) bishops (map :pos (filter #(= (:piece %) (if (= stm "w") \B \b)) board))]
(reduce into [] (map #(bishop-moves board %) bishops))
 ))


(defn find-queen-moves
[board]
(let [stm (:side-to-move (board 64)) queens (map :pos (filter #(= (:piece %) (if (= stm "w") \Q \q)) board))]
(reduce into []  ( into (map #(rook-moves board %) queens) (map #(bishop-moves board %) queens)))
 ))

(defn find-king-moves
[board]
(let [stm (:side-to-move (board 64))
pos  (first (map :pos (filter #(= (:piece %) (if (= stm "w") \K \k)) board)))
 isopposite?  (if (= stm "w") isblack? iswhite?) 
issame-color?  (if (= stm "w") iswhite?  isblack?)
      file (if pos (rem pos 8) -100) rank (if pos (quot pos 8) -100) 
ranks (filter #(and (>= % 0) (< % 8)) [(- rank 1) rank (+ rank 1) ])
files (filter #(and (>= % 0) (< % 8)) [(- file 1) file (+ file 1) ])
]
  (if  (not pos)
    []
    (into
     (for [x1 (vec files) x2 (vec ranks) 
           :let [p2 (+ x1 (* x2 8))]
           :when (not (or (and (= x1 file) (= x2 rank))
                          (issame-color? board p2)))] (list pos p2)) ;regular non-castling moves
     (filter identity [(if (and (isempty? board (inc pos)) (isempty? board (+ 2 pos)) (str/includes? (:castles (board 64)) (if (= stm "w") "K" "k"))) (list pos (+ 2 pos)))
                       (if (and (isempty? board (dec pos)) (isempty? board (- pos 2))  (isempty? board (- pos 3)) (str/includes? (:castles (board 64)) (if (= stm "w") "Q" "q")))  (list pos (- pos 2)))]) ;castling moves
     ))))


  (defn all-piece-moves
    "list of all piece moves (without checking for checks)"
    [board]
(reduce into 
(find-knight-moves board)
[(find-rook-moves board)
(find-bishop-moves board)
(find-queen-moves board)
(find-king-moves board)
(find-pawn-moves board)])
      )

  (defn all-enemy-captures
    "list of all piece moves (without checking for checks)"
    [board1]
  (let [board (move-piece board1 0 0) stm (:side-to-move (board 64)) pawns  (map :pos (filter #(= (:piece %) (if (= stm "w") \P \p)) board))]
(reduce into 
(find-knight-moves board)
[(find-rook-moves board)
(find-bishop-moves board)
(find-queen-moves board)
(find-king-moves board)
(reduce #(into %1 (pawn-captures %2 stm)) [] pawns)])
      ))

 (defn all-captures
    "list of all piece moves (without checking for checks)"
    [board]
  (let [stm (:side-to-move (board 64)) pawns  (map :pos (filter #(= (:piece %) (if (= stm "w") \P \p)) board))]
(reduce into 
(find-knight-moves board)
[(find-rook-moves board)
(find-bishop-moves board)
(find-queen-moves board)
(find-king-moves board)
(reduce #(into %1 (pawn-captures %2 stm)) [] pawns)])
      ))

(defn attacked-king-square
[board]
(for [king-moves (find-king-moves board)
               enemy-attacks (all-enemy-captures board)
:when (= (second king-moves) (rem (second enemy-attacks) 100))]
(second king-moves)
))

(defn ischeck?
[board]
 (let [ stm (:side-to-move (board 64))
kingpos  (first (map :pos (filter #(= (:piece %) (if (= stm "w") \K \k)) board)))  enemy-attacks (all-enemy-captures board)]
   (some #(= kingpos (rem (second %) 100)) enemy-attacks) 
))

(defn ischeck-move?
[board]
 (let [ stm (:side-to-move (board 64))
kingpos  (first (map :pos (filter #(= (:piece %) (if (= stm "w") \k \K)) board)))  enemy-attacks (all-captures board)]
   (some #(= kingpos (rem (second %) 100)) enemy-attacks) 
))

   (defn valid-moves
[board]
(let [stm (:side-to-move (board 64))
pos  (first (map :pos (filter #(= (:piece %) (if (= stm "w") \K \k)) board)))]
;  (apply vector)
  (for [move  (all-piece-moves board)
        :when  (and (or (< (second move) 64) (> (second move) 99))  (not (ischeck-move? (move-piece board (first move) (second move)))))]

    move
    )))

  (defn valid-moves2 "faster than valid-moves, but doesn't include checks, so used only for simulations to save time"
[board]
(let [stm (:side-to-move (board 64))
pos  (first (map :pos (filter #(= (:piece %) (if (= stm "w") \K \k)) board)))]
 
 (filter  #(and (or (< (second %) 64) (> (second %) 99)) (> (second %) -1))  (all-piece-moves board))
))

(defn ismate?
[board]
  (and  (ischeck? board) (empty? (valid-moves board)))
)


(defn game-result
[board]
(if (ismate? board)
  (let [stm (:side-to-move (board 64))]
(if (= stm "w")
0; white loses
1; white wins
)
)
  (if (let [tpv (total-pv board)]
        (or (= tpv 3) (= tpv 0) (empty? (valid-moves2 board))
)
)
1/2 ;draw
;??? add stalemate
)))




