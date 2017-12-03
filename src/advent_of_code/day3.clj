; 65  64  63  62  61  60  59  58  57
; 66  37  36  35  34  33  32  31  56
; 67  38  17  16  15  14  13  30  55
; 68  39  18   5   4   3  12  29  54
; 69  40  19   6   1   2  11  28  53
; 70  41  20   7   8   9  10  27  52
; 71  42  21  22  23  24  25  26  51
; 72  43  44  45  46  47  48  49  50
; 73  74  75  76  77  78  79  80  81
;
; 8   7   6   5   4   5   6   7   8
; 7   6   5   4   3   4   5   6   7
; 6   5   4   3   2   3   4   5   6
; 5   4   3   2   1   2   3   4   5
; 4   3   2   1   0   1   2   3   4
; 5   4   3   2   1   2   3   4   5
; 6   5   4   3   2   3   4   5   6
; 7   6   5   4   3   4   5   6   7
; 8   7   6   5   4   5   6   7   8
;
;     1 2 3 4 5 6 7 8   9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4   5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8
;     , . , . , . , .     ,   .   ,   .   ,   .   ,   .       ,     .     ,     .     ,     .     ,     .
; 0 | 1 2 1 2 1 2 1 2 | 3 2 3 4 3 2 3 4 3 2 3 4 3 2 3 4 | 5 4 3 4 5 6 5 4 3 4 5 6 5 4 3 4 5 6 5 4 3 4 5 6
; --|-----------------|---------------------------------|------------------------------------------------
;   | 1 2 3 4 5 6 7 8 | 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 | 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4    i
;   | 1   1   1   1   |   2   2   2   2   2   2   2   2 |     3     3     3     3     3     3     3     3    step
;   |   2   2   2   2 |       4       4       4       4 |           6           6           6           6    max
;
;
;       ,       .       ,       .       ,       .       ,       .
; 7 6 5 4 5 6 7 8 7 6 5 4 5 6 7 8 7 6 5 4 5 6 7 8 7 6 5 4 5 6 7 8
; ---------------------------------------------------------------
; 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2   i
;       4       4       4       4       4       4       4       4   step
;               8               8               8               8   max
;
; idx mod (4 * max) == 0 -> i = 1, step+1, max+2
; i mod max == 0 -> max
; i mod step == 0 -> step
; i mod max < step -> max - i
; i mod max > step -> step + (i mod step)

(ns advent-of-code.day3
  (:gen-class))

(use 'clojure.tools.trace)
(trace-vars advent-of-code.day3/mahnattan-distance)
(trace-vars advent-of-code.day3/calculate-distance)
(trace-vars clojure.core/mod)
(untrace-vars advent-of-code.day3/mahnattan-distance)
(untrace-vars advent-of-code.day3/calculate-distance)
(untrace-vars clojure.core/mod)

(defn calculate-distance [i step max]
  (cond
    (= 0 (mod i max))     max
    (= 0 (mod i step))    step
    (> step (mod i max))  (- max (mod i max))
    :else                 (+ step (mod i step))))

(defn mahnattan-distance [input]
   (loop [target (dec input)
          index 1
          local-index 1
          step 1
          max 2]
     (if (= target index)
       (calculate-distance local-index step max)
       (if (= 0 (mod local-index (* 4 max)))
           (recur target (inc index) 1 (inc step) (+ max 2))
           (recur target (inc index) (inc local-index) step max)))))

(mahnattan-distance 361527)
; (map #(print % "-" (mahnattan-distance %) " ")
;   (range 2 30))

; ---- PART TWO ----
; [-1, 1] [0, 1] [1, 1]
; [-1, 0] [0, 0] [1, 0]
; [-1,-1] [0,-1] [1,-1]
