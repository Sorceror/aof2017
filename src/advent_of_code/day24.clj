(ns advent-of-code.day24
  (gen-class))

;--- Day 24: Electromagnetic Moat ---
;The CPU itself is a large, black building surrounded by a bottomless pit. Enormous metal tubes extend outward from the side of the building at regular intervals and descend down into the void. There's no way to cross, but you need to get inside.
;No way, of course, other than building a bridge out of the magnetic components strewn about nearby.
;Each component has two ports, one on each end. The ports come in all different types, and only matching types can be connected. You take an inventory of the components by their port types (your puzzle input). Each port is identified by the number of pins it uses; more pins mean a stronger connection for your bridge. A 3/7 component, for example, has a type-3 port on one side, and a type-7 port on the other.
;Your side of the pit is metallic; a perfect surface to connect a magnetic, zero-pin port. Because of this, the first port you use must be of type 0. It doesn't matter what type of port you end with; your goal is just to make the bridge as strong as possible.
;The strength of a bridge is the sum of the port types in each component. For example, if your bridge is made of components 0/3, 3/7, and 7/4, your bridge has a strength of 0+3 + 3+7 + 7+4 = 24.
;
;For example, suppose you had the following components:
;
;0/2
;2/2
;2/3
;3/4
;3/5
;0/1
;10/1
;9/10

(def test-input
  "0/2
   2/2
   2/3
   3/4
   3/5
   0/1
   10/1
   9/10")

;With them, you could make the following valid bridges:
;
;0/1
;0/1--10/1
;0/1--10/1--9/10
;0/2
;0/2--2/3
;0/2--2/3--3/4
;0/2--2/3--3/5
;0/2--2/2
;0/2--2/2--2/3
;0/2--2/2--2/3--3/4
;0/2--2/2--2/3--3/5
;(Note how, as shown by 10/1, order of ports within a component doesn't matter. However, you may only use each port on a component once.)

(use 'advent-of-code.utils)

(defn parse-input [input]
  (->> input
       (to-lines)
       (map #(re-matches #"(\d+)/(\d+)" %))
       (map #(drop 1 %))
       (map #(map (fn [p] (to-digit p)) %))
       (map set)
       (set)))

;Of these bridges, the strongest one is 0/1--10/1--9/10; it has a strength of 0+1 + 1+10 + 10+9 = 31.
;What is the strength of the strongest bridge you can make with the components you have available?


(defn get-connections [p ports]
  (->> ports
       (filter #(contains? % p))))

(defn backtracking-max [p ports acc]
  (let [connections (get-connections p ports)]
    (if (empty? connections)
        acc
        (reduce
          (fn [max c]
            (let [next-port (or (first (disj c p)) p)
                  max-val (backtracking-max next-port (disj ports c) (+ acc p next-port))]
              (if (> max-val max)
                  max-val
                  max)))
          acc connections))))

(backtracking-max 0 (parse-input test-input) 0)
;=> 31
(backtracking-max 0 (parse-input (slurp "src/advent_of_code/day24.in")) 0)
;=> 1859

;--- Part Two ---
;The bridge you've built isn't long enough; you can't jump the rest of the way.
;In the example above, there are two longest bridges:
;0/2--2/2--2/3--3/4
;0/2--2/2--2/3--3/5
;Of them, the one which uses the 3/5 component is stronger; its strength is 0+2 + 2+2 + 2+3 + 3+5 = 19.
;What is the strength of the longest bridge you can make? If you can make multiple bridges of the longest length, pick the strongest one.

(defn backtracking-max-len [p ports acc-max acc-len]
  (let [connections (get-connections p ports)]
    (if (empty? connections)
      [acc-max acc-len]
      (reduce
        (fn [[max len] c]
          (let [next-port (or (first (disj c p)) p)
                [max-val len-val] (backtracking-max-len next-port (disj ports c) (+ acc-max p next-port) (inc acc-len))]
            (if (> len-val len)
                [max-val len-val]
                (if (and (= len-val len) (> max-val max))
                  [max-val len-val]
                  [max len]))))
        [acc-max acc-len] connections))))

(backtracking-max-len 0 (parse-input test-input) 0 0)
; => [19 4]
(backtracking-max-len 0 (parse-input (slurp "src/advent_of_code/day24.in")) 0 0)
;=> [1799 35]