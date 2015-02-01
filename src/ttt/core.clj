(ns ttt.core
  (:gen-class))

(def size 3)
(def p1 :x)
(def p2 :o)

(defn empty-ttt-board []
  (vec (repeat size (vec (repeat size nil)))))

(defn diagonals []
  (let [rsize (range size)]
    [(vec (for [x rsize
                y rsize
                :when (= x y)]
            [x y]))
     (vec (for [x rsize
                y rsize
                :when (= (dec size) (+ x y))]
            [x y]))]))

(defn ttt-win [board]
  (some {(vec (repeat size :x)) :x
         (vec (repeat size :o)) :o}
        (concat board 
                (apply map list board)
                (for [d (diagonals)]
                  (for [y d]
                    (get-in board y))))))

(defn ttt-moves [board player]
  (for [x (range size)
        y (range size)
        :when (nil? (get-in board [x y]))]
    (assoc-in board [x y] player)))

(defn depth [board]
  (->> (flatten board)
       (filter identity)
       count))

(defn compare-ttt-boards [scored-boards]
  (sort-by #(* (- (inc (* size size))
                  (:depth %))
               (:score %))
           scored-boards))

(defn invert-player [whos-move]
  (if (= whos-move p1) p2 p1))

(defn max-ttt [& boards]
  (last (compare-ttt-boards boards)))

(defn min-ttt [& boards]
  (first (compare-ttt-boards boards)))

(defn score-ttt-board [board whos-move]
  (let [board-depth (depth board)
        board-winner (ttt-win board)
        next-moves (ttt-moves board whos-move)]
    (cond  
      board-winner {:board board
                    :depth board-depth
                    :score  (if (= board-winner p1) 1 -1)
                    :next-board nil}
      (= board-depth (* size size)) {:board board
                                     :depth board-depth
                                     :score 0
                                     :next-board nil}
      :else (let [scored-next-moves (map #(score-ttt-board
                                           %
                                           (invert-player whos-move))
                                         next-moves)
                  best-move (reduce 
                             (if (= whos-move p1)
                               max-ttt
                               min-ttt)
                             scored-next-moves)]
              {:board board
               :depth (:depth best-move)
               :score (:score best-move)
               :next-board best-move}))))

(defn next-ttt-move [board whos-move]
  (:board (:next-board (score-ttt-board board whos-move))))

(def play-ttt (memoize next-ttt-move))
