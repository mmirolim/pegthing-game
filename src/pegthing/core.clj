(ns pegthing.core
  (require [clojure.set :as set])
  (:gen-class)
  (:require [clojure.string :as str]))

(declare successful-move prompt-move user-entered-valid-move user-entered-invalid-move game-over query-rows prompt-rows)

;; TODO reuse std buffer
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, let's play Peg thing!")
  (prompt-rows))

(defn tri*
  "Generates lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn triangular?
  "Is the number triangular? e.g 1, 3, 6, 10, 15, etc"
  [n]
  (= n (last (take-while #(>= n %) tri))))

(defn row-tri
  "The triangular number at the end of row n"
  [n]
  (last (take n tri)))

(defn row-num
  "Returns row number the position belongs to: pos 1 in row 1,
  positions 2 and 3 in row 2, etc"
  [pos]
  (inc (count (take-while #(> pos %) tri))))

(defn connect
  "Form a mutual connection between two positions"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]])
    board))

(defn connect-right
  "Connects peg with right side destination if available"
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (inc pos)            ;neighbor position on right side
        destination (inc neighbor)    ;destination is next to neighbor
        ]
    (if (and
         (<= destination max-pos)
         (= row (row-num destination))
         )
      (connect board max-pos pos neighbor destination)
      board)))

(defn connect-down-left
  "Connects peg with down left destination if avaialble"
  [board max-pos pos ]
  (let [row (row-num pos)
        neighbor (+ row pos)
        destination (+ 1 row neighbor)
        ]
    (if (<= destination max-pos)
      (connect board max-pos pos neighbor destination)
      board)))

(defn connect-down-right
  "Connects peg with down right destination if avaialble"
  [board max-pos pos ]
  (let [row (row-num pos)
        neighbor (+ 1 row pos)
        destination (+ 2 row neighbor)
        ]
    (if (<= destination max-pos)
      (connect board max-pos pos neighbor destination)
      board)))


(defn add-pos
  "Pegs the position and performs connections"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connect-fn]
              (connect-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

(defn new-board
  "Creates a new board with the given number of rows"
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos]
              (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))


(defn pegged?
  "Does the position have a peg in it"
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  "Take the peg at given position out of the board"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Put a peg in the board at given position"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Take peg out of p1 and place it in p2"
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))

(defn valid-moves
  "Return a map of all valid moves for pos, where the key is 
  destination and the value is the jumped position"
  [board pos]
  (into {} (filter (fn [[destination jumped]]
                     (and (not (pegged? board destination))
                          (pegged? board jumped)))
                   (get-in board [pos :connections]))))

(defn valid-move?
  "Return jumpd position if the move from p1 to p2 is valid, nil
  otherwise"
  [board p1 p2]
  (get (valid-moves board p1) p2))

(defn make-move
  "Move peg from p1 to p2, removing jumped peg"
  [board p1 p2]
  (if-let [jumped (valid-move? board p1 p2)]
    (move-peg (remove-peg board jumped) p1 p2)))

(defn can-move?
  "Do any of the pegged position have valid moves?"
  [board]
  (some (comp not-empty (partial valid-moves board))
        (map first (filter #(get (second %) :pegged) board))))


(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)

(def ansi-styles
  {:red "[31m"
   :green "[32m"
   :blue "[34m"
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
  "Render board position"
  [board pos]
  (str (nth letters (dec pos))
       (if (get-in board [pos :pegged])
         (colorize "0" :blue)
         (colorize "*" :red))))

(defn row-positions
  "Return all positions in the given row"
  [row-num]
  (range (inc (or (row-tri (dec row-num)) 0))
         (inc (row-tri row-num))))

(defn row-padding
  "Pads the beginning of a string to center row"
  [row-num rows]
  (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
    (apply str (take pad-length (repeat " ")))))

(defn render-row
  "Create string which represents row"
  [board row-num]
  (str (row-padding row-num (:rows board))
       (clojure.string/join " " (map (partial render-pos board) (row-positions row-num)))))

(defn print-board
  "Prints board"
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (println (render-row board row-num))))

;; Player interactions
(defn letter->pos
  "Converts a letter string to the corresponding position"
  [letter]
  (inc (- (int (first letter)) alpha-start)))

(defn get-input
  "Waits for user to enter text and hit enter, then cleans the input"
  ([] (get-input nil))
  ([default]
   (let [input (str/trim (read-line))]
     (if (and (empty? input) (nil? default)) ; TODO handle proper exit with stats
       (do
         (println "Bye!")
         (System/exit 0)))
     (if (empty? input)
       default
       (str/lower-case input)))))

(defn characters-as-strings
  "Filters nonalphabetic characters from input and returns collection of letters"
  [string]
  (re-seq #"[a-zA-Z]" string))

(defn prompt-move
  "Reads the player's input and acts on it"
  [board]
  (println "\nHere's your board =>")
  (print-board board)
  (println "Move from where to where? Enter two letters =>")
  (let [input (map letter->pos (characters-as-strings (get-input)))]
    (if-let [new-board (make-move board (first input) (second input))]
      (user-entered-valid-move new-board)
      (user-entered-invalid-move board))))

(defn user-entered-valid-move
  "Handles the next step after a user has entered a valid move"
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

(defn user-entered-invalid-move
  "Handles the next step after a user has entered an invalid move"
  [board]
  (println (colorize "\nThis was an invalid move\n" :red))
  (prompt-move board))


(defn game-over
  "Announce the game is over and prompt to play again"
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println (colorize (str "Game over! You had " remaining-pegs " pegs left") :green))
    (print-board board)
    (println "Play again? y/n [y]")
    (let [input (get-input "y")]
      (if (= "y" input)
        (prompt-rows)
        (do
          (println "Bye!")
          (System/exit 0))))))


(defn prompt-empty-peg
  "Ask and removes peg from board"
  [board]
  (println "Here's your board")
  (print-board board)
  (println (colorize "Remove which peg? [e]" :green))
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))


(defn prompt-rows
  "Gets number of rows for a board from user"
  []
  (println (colorize "How many rows? [5]" :green))
  (let [rows (Integer. (get-input 5))
        board (new-board rows)]
    (if (>= rows 4)
      (prompt-empty-peg board)
      (do
        (println "Min rows number is 4")
        (prompt-rows)))))

