(ns advent-of-code.day11
  (gen-class))

;The hexagons ("hexes") in this grid are aligned such that adjacent hexes can be found to the north, northeast, southeast, south, southwest, and northwest:
;   \ n  /
; nw +--+ ne
;   /    \
; -+      +-
;   \    /
; sw +--+ se
;   / s  \
;You have the path the child process took. Starting where he started, you need to determine the fewest number of steps required to reach him. (A "step" means to move from the hex you are in to any adjacent hex.)

; Based on https://www.redblobgames.com/grids/hexagons/#distances
;         -1   x
;      \  +1   y  /
; -1 x  +--------+  +1 y
; +1 z /nw   n  ne\ -1 z
;    -+           +-
; -1 y \sw   s  se/ +1 x
; +1 z  +--------+  -1 z
;      /  +1   x  \
;         -1   y

(def directions {:n  [-1  1  0]
                 :s  [ 1 -1  0]
                 :nw [-1  0  1]
                 :ne [ 0  1 -1]
                 :sw [ 0 -1  1]
                 :se [ 1  0 -1]})

(defn add-vec [v1 v2]
  [(+ (get v1 0) (get v2 0)) (+ (get v1 1) (get v2 1)) (+ (get v1 2) (get v2 2))])

(defn move [current direction]
  (add-vec current (get directions direction)))

(defn parse-input [s]
  (->>
    (clojure.string/split s #",")
    (map #(clojure.string/trim %))
    (map #(keyword %))))

(defn abs [n]
  (max n (- n)))

(defn compute-distance [p1 p2]
  (/ (+ (abs (- (get p1 0) (get p2 0))) (abs (- (get p1 1) (get p2 1))) (abs (- (get p1 2) (get p2 2)))) 2))

(defn distance [start input]
  (loop [moves (parse-input input)
         position start]
    (if (empty? moves)
        (compute-distance start position)
        (recur (drop 1 moves) (move position (first moves))))))

(distance [0 0 0] "ne,ne,ne")
(distance [0 0 0] "ne,ne,sw,sw")
(distance [0 0 0] "ne,ne,s,s")
(distance [0 0 0] "se,sw,se,sw,sw")
(distance [0 0 0] (slurp "src/advent_of_code/day11.in"))

; ---- PART TWO ----
; How many steps away is the furthest he ever got from his starting position?

(defn furthest-distance [start input]
  (loop [moves (parse-input input)
         position start
         furthest 0]
    (if (empty? moves)
      furthest
      (recur (drop 1 moves) (move position (first moves)) (max furthest (compute-distance position start))))))

(furthest-distance [0 0 0] (slurp "src/advent_of_code/day11.in"))