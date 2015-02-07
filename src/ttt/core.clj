(ns ttt.core
  (:gen-class))

(require '[ttt.ai :as ai])

(defn try-until [f message]
  "Keep trying until we get the input without errors."
  (try (f message)
       (catch Exception e
         (println "Try again - invalid format")
         (try-until f message))))

(defn get-player-input-as-integer [message]
  "Get the input as an integer corresponding to the message"
  (println message)
  (Integer/parseInt (read-line)))

(defn check-choice-is-valid [choice coll]
  "Make sure the input is within bounds, and if so, return the input"
  (some (set coll) (vector choice)))

(defn get-choice [message coll]
  "Get the user to make a choice. Check that the choice is an integer, and is valid, i.e. - is in the coll"
  (println coll)
  (if-let [choice (check-choice-is-valid
                   (try-until get-player-input-as-integer
                              message)
                   coll)]
    choice
    (do
      (println "Try again")
      (recur message coll))))

(defn stringify-board [board]
  (apply str (interpose "\n" board)))

(defn play-game
  ([player board]
   "Check if the game is already over. If so, print the board and exit. Otherwise check if the game will be over by making a move (no need for opponent move). If so then make the move and exit. Finally, if both of these fail - make a move, print it, and get opponent to make a move. Implemented in reverse"
   
   (let [new-strategy (ai/play-ttt board player)]
     (cond
       (:next-board (:next-board new-strategy)) 
       (let [new-board (:board (:next-board new-strategy))]
         (println (stringify-board new-board))
         (let [available-moves (map inc (keep-indexed #(if (nil? %2) %1)
                                                      (flatten new-board)))
               opp-move (dec (get-choice "Make your move from these available moves"
                                         available-moves))
               opp-board (assoc-in new-board ((juxt quot rem) opp-move ai/size)
                                   (if (= player ai/p1)
                                     ai/p2
                                     ai/p1))]
           (play-game player opp-board)))
       
       (:next-board new-strategy) (println (stringify-board (:board (:next-board new-strategy))))
       
       :else (println (stringify-board board)))))

  ([player] (play-game player (ai/empty-ttt-board))))

(defn -main []
  (let [init-choice (get-choice "Enter 1 to start new-game with you starting. \nEnter 2 to start new-game with computer starting. Be patient the first time you choose this. \nEnter 3 to exit. \nStrategy is memoized so performance will pick up for >1 games" [1 2 3])]
    (case init-choice
      2 (do 
          (play-game ai/p1) 
          (println "Game Over") 
          (recur))
      1 (do
          (println (stringify-board (ai/empty-ttt-board)))
          (let [first-move (dec (get-choice "Make first move, from these available moves" 
                                            (range 1 (inc (* ai/size ai/size)))))]
            (play-game ai/p2 (assoc-in (ai/empty-ttt-board)
                                       ((juxt quot rem) first-move ai/size)
                                       ai/p1)))
          (println "Game Over")
          (recur))
      3 nil)))
