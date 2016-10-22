(ns pegthing.core
  (require [clojure.set :as set])
  (:gen-class))

(declare successful-move prompt-move game-over query-rows)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

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
      (connect board max-pos pos neighbor destination))))

(defn connect-down-right
  "Connects peg with down right destination if avaialble"
  [board max-pos pos ]
  (let [row (row-num pos)
        neighbor (+ 1 row pos)
        destination (+ 2 row neighbor)
        ]
    (if (<= destination max-pos)
      (connect board max-pos pos neighbor destination))))


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
    (println max-pos initial-board)
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))


(defn fib
  "Gnerates lazy sequence of fibonacci numbers"
  ([] (fib 1 1))
  ([n-2 n-1]
   (cons n-2 (lazy-seq (fib n-1 (+ n-1 n-2))))))
