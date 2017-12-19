(ns advent-of-code.day19
  (gen-class))

;--- Day 19: A Series of Tubes ---
;Somehow, a network packet got lost and ended up here. It's trying to follow a routing diagram (your puzzle input), but it's confused about where to go.
;Its starting point is just off the top of the diagram. Lines (drawn with |, -, and +) show the path it needs to take, starting by going down onto the only line connected to the top of the diagram. It needs to follow this path until it reaches the end (located somewhere within the diagram) and stop there.
(defn find-start [tubes]
  (let [first-row (first tubes)]
    [(.indexOf first-row \|) 0]))
;Sometimes, the lines cross over each other; in these cases, it needs to continue going the same direction, and only turn left or right when there's no other option. In addition, someone has left letters on the line; these also don't change its direction, but it can use them to keep track of where it's been. For example:
;
(def test-input (slurp "src/advent_of_code/day19-test.in"))

(defn parse-tubes [input]
  (->> (clojure.string/split-lines input)
       (map #(vec (char-array %)))
       (vec)))

(defn get-pos
  ([pos tubes] (get-pos (first pos) (second pos) tubes))
  ([x y tubes]
   (if (or (< x 0) (>= x (count (first tubes))) (< y 0) (>= y (count tubes)))
       \space
       (nth (nth tubes y) x))))

;    |
;    |  +--+
;    A  |  C
;F---|----E|--+
;    |  |  |  D
;    +B-+  +--+
;
;Given this diagram, the packet needs to take the following path:
(def directions {:up    [ 0 -1]
                 :down  [ 0  1]
                 :right [ 1  0]
                 :left  [-1  0]})

(defn with-direction [posX posY direction]
  [(+ posX (first (get directions direction))) (+ posY (second (get directions direction)))])

(defn get-next-direction [last-direction pX pY tubes]
  (cond
    (or (= last-direction :up) (= last-direction :down))
    (let [left-char (get-pos (with-direction pX pY :left) tubes)]
      (if (not= left-char \space)
          :left
          :right))
    (or (= last-direction :left) (= last-direction :right))
    (let [right-char (get-pos (with-direction pX pY :up) tubes)]
      (if (not= right-char \space)
          :up
          :down))))

(defn move-step [direction posX posY tubes]
  (let [pX (+ posX (first (get directions direction)))
        pY (+ posY (second (get directions direction)))
        c (get-pos pX pY tubes)]
    (cond
      (= c \+) [pX pY (get-next-direction direction pX pY tubes) nil]
      (Character/isLetter ^char c) [pX pY direction c]
      (= c \space) [pX pY :none nil]
      :else [pX pY direction nil])))

(defn walk-though [tubes]
  (let [[pX pY] (find-start tubes)]
    (loop [posX pX
           posY pY
           direction :down
           s []]
      (if (= direction :none)
          s
          (let [[npX npY nDir found] (move-step direction posX posY tubes)]
            (recur npX npY nDir (conj s found)))))))

(defn solve1 [input]
  (->> (parse-tubes input)
       (walk-though)
       (apply str)))

;Starting at the only line touching the top of the diagram, it must go down, pass through A, and continue onward to the first +.
;Travel right, up, and right, passing through B in the process.
;Continue down (collecting C), right, and up (collecting D).
;Finally, go all the way left through E and stopping at F.
;Following the path to the end, the letters it sees on its path are ABCDEF.
(solve1 (slurp "src/advent_of_code/day19-test.in"))
;=> "ABCDEF"

;The little packet looks up at you, hoping you can help it find the way. What letters will it see (in the order it would see them) if it follows the path? (The routing diagram is very wide); make sure you view it without line wrapping.)
(solve1 (slurp "src/advent_of_code/day19.in"))
;=> "GEPYAWTMLK"

;--- Part Two ---
;The packet is curious how many steps it needs to go.
(defn walk-though2 [tubes]
  (let [[pX pY] (find-start tubes)]
    (loop [posX pX
           posY pY
           direction :down
           steps 0]
      (if (= direction :none)
        steps
        (let [[npX npY nDir _] (move-step direction posX posY tubes)]
          (recur npX npY nDir (inc steps)))))))

(defn solve2 [input]
  (->> (parse-tubes input)
       (walk-though2)))
;For example, using the same routing diagram from the example above...
;
;    |
;    |  +--+
;    A  |  C
;F---|----E|--+
;    |  |  |  D
;    +B-+  +--+
;
;...the packet would go:
;
;6 steps down (including the first line at the top of the diagram).
;3 steps right.
;4 steps up.
;3 steps right.
;4 steps down.
;3 steps right.
;2 steps up.
;13 steps left (including the F it stops on).
;This would result in a total of 38 steps.
(solve2 (slurp "src/advent_of_code/day19-test.in"))
;=> 38

;How many steps does the packet need to go?
(solve2 (slurp "src/advent_of_code/day19.in"))
;=> 17628
