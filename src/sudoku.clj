(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

;; (def sudoku-board
;;   (board [[5 3 0 0 7 0 0 0 0]
;;           [6 0 0 1 9 5 0 0 0]
;;           [0 9 8 0 0 0 0 6 0]
;;           [8 0 0 0 6 0 0 0 3]
;;           [4 0 0 8 0 3 0 0 1]
;;           [7 0 0 0 2 0 0 0 6]
;;           [0 6 0 0 0 0 2 8 0]
;;           [0 0 0 4 1 9 0 0 5]
;;           [0 0 0 0 8 0 0 7 9]]))

;; (def solved-board
;;   (board [[5 3 4 6 7 8 9 1 2]
;;           [6 7 2 1 9 5 3 4 8]
;;           [1 9 8 3 4 2 5 6 7]
;;           [8 5 9 7 6 1 4 2 3]
;;           [4 2 6 8 5 3 7 9 1]
;;           [7 1 3 9 2 4 8 5 6]
;;           [9 6 1 5 3 7 2 8 4]
;;           [2 8 7 4 1 9 6 3 5]
;;           [3 4 5 2 8 6 1 7 9]]))

;; Write the function (value-at board coordinates) that returns the value at coordinate in board:
;; (value-at sudoku-board [0 1]) ;=> 3
;; (value-at sudoku-board [0 0]) ;=> 5
(defn value-at [board coord]
  (get-in board coord))

;; Write the function (has-value? board coordinates) that returns false if the square at coordinates
;; is empty (has 0), and otherwise true.
;; (has-value? sudoku-board [0 0]) ;=> true
;; (has-value? sudoku-board [0 2]) ;=> false
(defn has-value? [board coord]
  (> (value-at board coord) 0))

;; Write the function (row-values board coordinates) that returns a set with all numbers on the row of the coordinates
;; Remember that you can use destructing inside the parameter vector to get the row.
;; (row-values sudoku-board [0 2]) ;=> #{0 5 3 7}
;; (row-values sudoku-board [3 2]) ;=> #{0 8 6 3}
(defn row-values [board [row _]]
  (set (get board row)))

;; Write the function (col-values board coordinates) that returns a set with all numbers on the col of the coordinates
;; (col-values sudoku-board [0 2]) ;=> #{0 8}
;; (col-values sudoku-board [4 8]) ;=> #{3 1 6 0 5 9}
(defn col-values [board [_ col]]
  (set
    (map
      (fn [row] (value-at board [row col]))
      (range 0 9))))

;; Write the function (coord-pairs coord-sequence) that returns all coordinate-pairs of the form [row col]
;; where row is from coord-sequence and col is from coord-sequence.
;; (coord-pairs [0 1])   ;=> [[0 0] [0 1]
;;                       ;    [1 0] [1 1]]

;; (coord-pairs [0 1 2]) ;=> [[0 0] [0 1] [0 2]
;;                       ;    [1 0] [1 1] [1 2]
;;                       ;    [2 0] [2 1] [2 2]]
(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

;; Write the function (block-values board coordinates) that returns a set with all numbers in the block of coordinates.
;; (block-values sudoku-board [0 2]) ;=> #{0 5 3 6 8 9}
;; (block-values sudoku-board [4 5]) ;=> #{0 6 8 3 2}
;; You might want to write a helper function that returns the coordinates for the top left corner of the block.
;; (block-top-left-corner [0 2]) ; => [0 0]
;; (block-top-left-corner [8 8]) ; => [6 6]
(defn floor [number]
  (int (Math/floor number)))

(defn block-coord [coord]
  (* 3 (floor (/ coord 3))))

(defn block-top-left-corner [[row col]]
  [(block-coord row) (block-coord col)])

;; (block-coords (block-top-left-corner [8 8]))
(defn block-coords [[row col]]
  (for [row (range row (+ row 3))
        col (range col (+ col 3))]
    [row col]))

(defn block-values [board coord]
  (set
    (map
      (fn [coord] (value-at board coord))
      (block-coords (block-top-left-corner coord)))))

;; Write the function (valid-values-for board coordinates) that returns a set with all valid numbers for the square at
;; coordinates.
;; If the square at coordinates already has a value, valid-values should return the empty set #{}.
;; Remember that we already defined the set all-values.
;; (valid-values-for sudoku-board [0 0]) ;=> #{}
;; (valid-values-for sudoku-board [0 2]) ;=> #{1 2 4})
(defn reserved-values-for [board coord]
  (set/union
    (row-values board coord)
    (col-values board coord)
    (block-values board coord)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
      all-values
      (reserved-values-for board coord))))

;; Write the function (filled? board) which returns true if there are no empty squares in board, and otherwise false.
;; It might help to write a helper function that returns all numbers of the board in a sequence.
;; Remember that (contains? set element) can be used to check if element is in set.
;; (filled? sudoku-board) ;=> false
;; (filled? solved-board) ;=> true

;; It might help to write a helper function that returns all numbers of the board in a sequence.
(defn all-numbers [board]
  (apply concat (map concat board)))

(defn filled? [board]
  (not (contains?
         (set (all-numbers board))
         0)))

;; Write the function (rows board) that returns a sequence of value sets for each row of board.
;; That is, the first set in (rows board) is a set that has every element of the first row of board as
;; element and so on.
;; (rows sudoku-board) ;=> [#{5 3 0 7}
;;                     ;    #{6 0 1 9 5}
;;                     ;    #{0 9 8 6}
;;                     ;    #{8 0 6 3}
;;                     ;    #{4 0 8 3 1}
;;                     ;    #{7 0 2 6}
;;                     ;    #{0 6 2 8}
;;                     ;    #{0 4 1 9 5}
;;                     ;    #{0 8 7 9}]

;; (rows solved-board) ;=> [#{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}]

(defn rows [board]
  (map set board))


;; Write the function (valid-rows? board) that returns true if every row in board is a valid filled row.
;; (valid-rows? solved-board)  ;=> truthy
;; (valid-rows? sudoku-board) ;=> falsey


(defn valid-x? [board f]
  (every?
    (fn [x] (= x all-values))
    (f board)))

(defn valid-rows? [board]
  (valid-x? board rows))

;; Write the function (cols board) that returns the values of each column in board as a sequence of sets.
;; (cols sudoku-board) ;=> [#{5 6 0 8 4 7}
;;                     ;    #{3 0 9 6}
;;                     ;    #{0 8}
;;                     ;    #{0 1 8 4}
;;                     ;    #{7 9 0 6 2 1 8}
;;                     ;    #{0 5 3 9}
;;                     ;    #{0 2}
;;                     ;    #{0 6 8 7}
;;                     ;    #{0 3 1 6 5 9}]

;; (cols solved-board) ;=> [#{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}]

(defn cols [board]
  (map
    (fn [col] (col-values board [0 col]))
    (range 0 9)))

;; Write the function (valid-cols? board) that returns true if every row in board is a valid filled column.
;; (valid-cols? solved-board)  ;=> truthy
;; (valid-cols? sudoku-board) ;=> falsey

(defn valid-cols? [board]
  (valid-x? board cols))


;; Write the function (blocks board) that returns the values of each block in board as a sequence of sets.
(defn blocks [board]
  (map
    (fn [coord] (block-values board coord))
    (coord-pairs [0 3 6])))

;; Write the function (valid-blocks? board) that returns true if every block in board is a valid filled block.
;; (valid-blocks? solved-board)  ;=> truthy
;; (valid-blocks? sudoku-board) ;=> falsey

(defn valid-blocks? [board]
  (valid-x? board blocks))

;; Write the function (valid-solution? board) that returns true if board is a valid solution to sudoku.
;; (valid-solution? solved-board)  ;=> truthy
;; (valid-solution? sudoku-board) ;=> falsey)
(defn valid-solution? [board]
  (and
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

;; Write the function (set-value-at board coord new-value) that changes the value at coord in board to new-value.
;; (def before-change
;;   (board [[5 3 0 0 7 0 0 0 0]
;;           [6 0 0 1 9 5 0 0 0]
;;           [0 9 8 0 0 0 0 6 0]
;;           [8 0 0 0 6 0 0 0 3]
;;           [4 0 0 8 0 3 0 0 1]
;;           [7 0 0 0 2 0 0 0 6]
;;           [0 6 0 0 0 0 2 8 0]
;;           [0 0 0 4 1 9 0 0 5]
;;           [0 0 0 0 8 0 0 7 9]]))
;; (def after-change
;;   (board [[5 3 0 0 7 0 0 0 0]
;;           [6 0 0 1 9 5 0 0 0]
;;           [0 4 8 0 0 0 0 6 0]
;;           [8 0 0 0 6 0 0 0 3]
;;           [4 0 0 8 0 3 0 0 1]
;;           [7 0 0 0 2 0 0 0 6]
;;           [0 6 0 0 0 0 2 8 0]
;;           [0 0 0 4 1 9 0 0 5]
;;           [0 0 0 0 8 0 0 7 9]]))
;; (= after-change (set-value-at before-change [2 1] 4))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

;; Write the function (find-empty-point board) that returns coordinates to an empty point
;; (that is, in our representation has value 0).
;; (find-empty-point sudoku-board)

(defn empty-points [board]
  (filter
    (fn [coord] (zero? (value-at board coord)))
    (coord-pairs (range 0 9))))

(defn find-empty-point [board]
  (first
    (empty-points board)))



;; Write the function (solve board) which takes a sudoku board as a parameter and returns a valid solution to
;; the given sudoku.
;;   (solve sudoku-board) => solved-board
;; Recap of backtracking:
;; check if you are at the end
;; if so, is the solution valid?
;;   if not, return an empty sequence
;;   otherwise return [solution]
;; if not
;;   select an empty location
;;   try solving with each valid value for that location

;; Finds first coordinate on the board that is a sure thing - there is only one possible number that can be placed there
;; (find-sure-move sudoku-board)
(defn find-sure-move [board]
  (first (filter
           (fn [coord] (= 1 (count (valid-values-for board coord))))
           (empty-points board))))

;; Solves an easy sudoku by finding the next sure move
(defn naive-solve [current-board]
  (if (filled? current-board)
    (if (valid-solution? current-board)
      current-board
      [])
    (let [sure-move (find-sure-move current-board)
          sure-move-coord (first (valid-values-for current-board sure-move))]
      (naive-solve
        (set-value-at
          current-board
          sure-move
          sure-move-coord)))))

;; (naive-solve sudoku-board)
;; (= (naive-solve sudoku-board) solved-board)

(defn solve [board]
  (naive-solve board))
