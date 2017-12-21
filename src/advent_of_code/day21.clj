(ns advent-of-code.day21
  (gen-class))

;--- Day 21: Fractal Art ---
;You find a program trying to generate some art. It uses a strange process that involves repeatedly enhancing the detail of an image through a set of rules.
;
;The image consists of a two-dimensional square grid of pixels that are either on (#) or off (.). The program always begins with this pattern:
;
;.#.
;..#
;###
;Because the pattern is both 3 pixels wide and 3 pixels tall, it is said to have a size of 3.
;
;Then, the program repeats the following process:
;
;If the size is evenly divisible by 2, break the pixels up into 2x2 squares, and convert each 2x2 square into a 3x3 square by following the corresponding enhancement rule.
;Otherwise, the size is evenly divisible by 3; break the pixels up into 3x3 squares, and convert each 3x3 square into a 4x4 square by following the corresponding enhancement rule.
;Because each square of pixels is replaced by a larger one, the image gains pixels and so its size increases.
;
;The artist's book of enhancement rules is nearby (your puzzle input); however, it seems to be missing rules. The artist explains that sometimes, one must rotate or flip the input pattern to find a match. (Never rotate or flip the output pattern, though.) Each pattern is written concisely: rows are listed as single units, ordered top-down, and separated by slashes. For example, the following rules correspond to the adjacent patterns:
;
; ../.#  =  ..
;           .#
;
;                 .#.
; .#./..#/###  =  ..#
;                 ###
;
;                         #..#
; #..#/..../#..#/.##.  =  ....
;                         #..#
;                         .##.
;When searching for a rule to use, rotate and flip the pattern as necessary. For example, all of the following patterns match the same rule:
;
; .#.   .#.   #..   ###
; ..#   #..   #.#   ..#
; ###   ###   ##.   .#.
(use 'advent-of-code.utils)

; rotate | 0 1 | 2 0 | flip | 0 1 | 1 0 |
;        | 2 3 | 3 1 |      | 2 3 | 3 2 |
;
; rotate | 0 1 2 | 6 3 0 | flip | 0 1 2 | 2 1 0 |
;        | 3 4 5 | 7 4 1 |      | 3 4 5 | 5 4 3 |
;        | 6 7 8 | 8 5 2 |      | 6 7 8 | 8 7 6 |

(def get-idx
  (memoize
    (fn [idx pattern]
      (loop [i 0
             count idx]
        (if (= (.charAt ^String pattern i) \/)
            (recur (inc i) count)
            (if (= count 0)
              (.charAt ^String pattern i)
              (recur (inc i) (dec count))))))))
      ;(-> (clojure.string/replace pattern #"\/" "")
      ;    (nth idx)))))

(defn pattern-size [str]
  (clojure.string/index-of ^String str "/"))

(defn rotate-pattern
  ([s]
   (let [size (pattern-size s)]
     (cond
       (= size 2)
       (str (get-idx 2 s) (get-idx 0 s) "/" (get-idx 3 s) (get-idx 1 s))
       (= size 3)
       (str (get-idx 6 s) (get-idx 3 s) (get-idx 0 s) "/" (get-idx 7 s) (get-idx 4 s) (get-idx 1 s) "/" (get-idx 8 s) (get-idx 5 s) (get-idx 2 s))
       :else
       (println "Size not supported!"))))
  ([count s]
   (loop [c 0
          p s]
     (if (= c count)
         p
         (recur (inc c) (rotate-pattern p))))))

(defn flip-pattern
  ([s]
   (let [size (pattern-size s)]
        (cond
          (= size 2)
          (str (get-idx 1 s) (get-idx 0 s) "/" (get-idx 3 s) (get-idx 2 s))
          (= size 3)
          (str (get-idx 2 s) (get-idx 1 s) (get-idx 0 s) "/" (get-idx 5 s) (get-idx 4 s) (get-idx 3 s) "/" (get-idx 8 s) (get-idx 7 s) (get-idx 6 s))
          :else
          (println "Size not supported!")))))

(defn generate-variations [str]
  (distinct
    (list
      str
      (rotate-pattern 1 str)
      (rotate-pattern 2 str)
      (rotate-pattern 3 str)
      (flip-pattern str)
      (rotate-pattern 1 (flip-pattern str))
      (rotate-pattern 2 (flip-pattern str))
      (rotate-pattern 3 (flip-pattern str)))))

(defn parse-rules [input]
  (->> input
       (to-lines)
       (map #(re-matches #"([\.\#\/]+)\s=>\s([\.\#\/]+)" %))
       (map #(drop 1 %))
       (map #(hash-map :from (first %) :original (first %) :to (second %) :rule-size (pattern-size (first %))))
       (map #(assoc % :from (generate-variations (get % :from))))))

;Suppose the book contained the following two rules:
;
;../.# => ##./#../...
;.#./..#/### => #..#/..../..../#..#
;As before, the program begins with this pattern:
;
; .#.
; ..#
; ###
; The size of the grid (3) is not divisible by 2, but it is divisible by 3. It divides evenly into a single square; the square matches the second rule, which produces:
;
; #..#
; ....
; ....
; #..#
; The size of this enhanced grid (4) is evenly divisible by 2, so that rule is used. It divides evenly into four squares:
;
; #.|.#
; ..|..
; --+--
; ..|..
; #.|.#
; Each of these squares matches the same rule (../.# => ##./#../...), three of which require some flipping and rotation to line up with the rule. The output for the rule is the same in all four cases:
;
; ##.|##.
; #..|#..
; ...|...
; ---+---
; ##.|##.
; #..|#..
; ...|...
;Finally, the squares are joined into a new grid:
;
; ##.##.
; #..#..
; ......
; ##.##.
; #..#..
; ......
;Thus, after 2 iterations, the grid contains 12 pixels that are on.
;
;How many pixels stay on after 5 iterations?
(def test-input
  "../.# => ##./#../...
  .#./..#/### => #..#/..../..../#..#")

(def base-pattern ".#./..#/###")

(defn get-block [idx size columns board]
  (let [block-column (mod idx columns)
        block-row (quot idx columns)
        tl1-idx (+ (* block-column size) (* block-row columns size size))
        tl2-idx (+ tl1-idx (* columns size))
        tl3-idx (+ tl2-idx (* columns size))]
        ;_ (println block-column block-row tl1-idx tl2-idx tl3-idx)]
    (cond
      (= 2 size)
      (str
        (get-idx tl1-idx board) (get-idx (inc tl1-idx) board) "/"
        (get-idx tl2-idx board) (get-idx (inc tl2-idx) board))
      (= 3 size)
      (str
        (get-idx tl1-idx board) (get-idx (+ tl1-idx 1) board) (get-idx (+ tl1-idx 2) board) "/"
        (get-idx tl2-idx board) (get-idx (+ tl2-idx 1) board) (get-idx (+ tl2-idx 2) board) "/"
        (get-idx tl3-idx board) (get-idx (+ tl3-idx 1) board) (get-idx (+ tl3-idx 2) board)))))

(defn get-blocks [block-size board]
  (let [columns (quot (pattern-size board) block-size)
        blocks-count (* columns columns)]
    (loop [blocks '()
           block-idx (dec blocks-count)]
      (if (< block-idx 0)
          blocks
          (recur (conj blocks (get-block block-idx block-size columns board)) (dec block-idx))))))

(def transform-block
  (memoize
    (fn [block-size block rules]
        (->> (filter (fn [rule] (= block-size (get rule :rule-size))) rules)
             (pmap (fn [rule] (map (fn [rule-var] [rule-var (get rule :to)]) (get rule :from))))
             (apply concat)
             (filter #(= block (first %)))
             ;; get first positive result
             (first)
             ;; extract value from result
             (second)))))

(defn transform [block-size rules board]
  (let [columns (quot (pattern-size board) block-size)]
       (->>
         (get-blocks block-size board)
         (pmap #(transform-block block-size % rules))
         (partition columns)
         (pmap (fn [block] (map #(clojure.string/split % #"\/") block)))
         (pmap (fn [rows] (apply mapv str rows)))
         (flatten)
         (clojure.string/join "/"))))

(defn step [rules board]
  (let [row-size (pattern-size board)]
    (cond
      (= 0 (mod row-size 2)) (transform 2 rules board)
      (= 0 (mod row-size 3)) (transform 3 rules board))))

(defn iterate-board [total rules board]
  (loop [c total
         b board]
    (println c)
    (if (= c 0)
        b
        (recur (dec c) (step rules b)))))

(defn count-on-pixels [board]
  (->> board
       (filter #(= \# %))
       (count)))

(->> base-pattern
     (iterate-board 5 (parse-rules (slurp "src/advent_of_code/day21.in")))
     (count-on-pixels))
;=> 123
;(->> base-pattern
;     (iterate-board 18 (parse-rules (slurp "src/advent_of_code/day21.in")))
;     (count-on-pixels))
; takes for ever!!
;=> 1984683