(def input
  "0
  3
  0
  1
  -3")

(defn to-digits [row]
  (map #(Integer/parseInt %)
    row))

(defn parse [s]
  (->>
    (clojure.string/split-lines s)
    (map #(clojure.string/trim %))
    (map #(clojure.string/split % #"\s"))
    (map #(to-digits %))
    (flatten)
    (vec)))

(defn diff-offset1 [val]
  (inc val))

(defn jump [input fn-offset]
   (loop [step 0
          index 0
          input (parse input)]
     (let [pos (get input index)]
       (if (nil? pos)
         step
         (recur (inc step) (+ index pos) (assoc input index (fn-offset pos)))))))

(jump input diff-offset1)
(jump (slurp "src/advent_of_code/day5.in") diff-offset1)

; ---- PART TWO ----

(defn diff-offset2 [val]
  (if (<= 3 val)
      (dec val)
      (inc val)))

(jump input diff-offset2)
(jump (slurp "src/advent_of_code/day5.in") diff-offset2)
