(ns advent-of-code.day24
  (gen-class))

(use 'advent-of-code.utils)

(def test-input
  "0/2
   2/2
   2/3
   3/4
   3/5
   0/1
   10/1
   9/10")

(defn parse-input [input]
  (->> input
       (to-lines)
       (map #(re-matches #"(\d+)/(\d+)" %))
       (map #(drop 1 %))
       (map #(map (fn [p] (to-digit p)) %))
       (map set)
       (set)))

(defn get-connections [p ports]
  (->> ports
       (filter #(contains? % p))))
       ;(map
       ;  (fn [port]
       ;    (or (first (disj port p)) p)))
       ;(set)))
       ; 0 can be only at the beginning of bridge
       ;(filter #(not= (second %) 0))))

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