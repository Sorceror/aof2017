(ns advent-of-code.day17
  (gen-class))

;--- Day 17: Spinlock ---
;Suddenly, whirling in the distance, you notice what looks like a massive, pixelated hurricane: a deadly spinlock. This spinlock isn't just consuming computing power, but memory, too; vast, digital mountains are being ripped from the ground and consumed by the vortex.
;If you don't move quickly, fixing that printer will be the least of your problems.
;This spinlock's algorithm is simple but efficient, quickly consuming everything in its path. It starts with a circular buffer containing only the value 0, which it marks as the current position. It then steps forward through the circular buffer some number of steps (your puzzle input) before inserting the first new value, 1, after the value it stopped on. The inserted value becomes the current position. Then, it steps forward from there the same number of steps, and wherever it stops, inserts after it the second new value, 2, and uses that as the new current position again.
;It repeats this process of stepping forward, inserting a new value, and using the location of the inserted value as the new current position a total of 2017 times, inserting 2017 as its final operation, and ending with a total of 2018 values (including 0) in the circular buffer.

(defn get-position [current steps spinlock]
  (mod (+ current steps) (count spinlock)))

(defn insert-after [position value spinlock]
  (vec (concat [] (take (inc position) spinlock) [value] (take-last (- (count spinlock) (inc position)) spinlock))))

(defn process-spinlock [count num-steps]
  (loop [spinlock [0]
         current 0
         value 1]
    (if (= (mod value 250000) 0) (println value))
    (if (<= value count)
        (let [next-pos (get-position current num-steps spinlock)]
             (recur (insert-after next-pos value spinlock) (inc next-pos) (inc value)))
        [spinlock current])))

;For example, if the spinlock were to step 3 times per insert, the circular buffer would begin to evolve like this (using parentheses to mark the current position after each iteration of the algorithm):
;
;(0), the initial state before any insertions.
(process-spinlock 0 3)
;=> [[0] 0]
;0 (1): the spinlock steps forward three times (0, 0, 0), and then inserts the first value, 1, after it. 1 becomes the current position.
(process-spinlock 1 3)
;=> [[0 1] 1]
;0 (2) 1: the spinlock steps forward three times (0, 1, 0), and then inserts the second value, 2, after it. 2 becomes the current position.
(process-spinlock 2 3)
;=> [[0 2 1] 1]
;0  2 (3) 1: the spinlock steps forward three times (1, 0, 2), and then inserts the third value, 3, after it. 3 becomes the current position.
(process-spinlock 3 3)
;=> [[0 2 3 1] 2]
;And so on:
;
(process-spinlock 4 3)
;0  2 (4) 3  1
;=> [[0 2 4 3 1] 2]
(process-spinlock 5 3)
;0 (5) 2  4  3  1
;=> [[0 5 2 4 3 1] 1]
(process-spinlock 6 3)
;0  5  2  4  3 (6) 1
;=> [[0 5 2 4 3 6 1] 5]
(process-spinlock 7 3)
;0  5 (7) 2  4  3  6  1
;=> [[0 5 7 2 4 3 6 1] 2]
(process-spinlock 8 3)
;0  5  7  2  4  3 (8) 6  1
;=> [[0 5 7 2 4 3 8 6 1] 6]
(process-spinlock 9 3)
;0 (9) 5  7  2  4  3  8  6  1
;=> [[0 9 5 7 2 4 3 8 6 1] 1]

;Eventually, after 2017 insertions, the section of the circular buffer near the last insertion looks like this:
(process-spinlock 2017 3)
;1512  1134  151 (2017) 638  1513  851
;=>[...850 478 2016 1512 1134 151 2017 638 1513 851 1135 269...] 1530]
;Perhaps, if you can identify the value that will ultimately be after the last value written (2017), you can short-circuit the spinlock. In this example, that would be 638.
(defn get-next-after-process [count step-num]
  (let [result (process-spinlock count step-num)]
    (get (first result) (inc (second result)))))

(get-next-after-process 2017 3)
;=> 638
;What is the value after 2017 in your completed circular buffer?
;Your puzzle input is 359.
(get-next-after-process 2017 359)
;=> 1506

;--- Part Two ---
;The spinlock does not short-circuit. Instead, it gets more angry. At least, you assume that's what happened; it's spinning significantly faster than it was a moment ago.
;You have good news and bad news.
;The good news is that you have improved calculations for how to stop the spinlock. They indicate that you actually need to identify the value after 0 in the current state of the circular buffer.
;The bad news is that while you were determining this, the spinlock has just finished inserting its fifty millionth value (50000000).
;What is the value after 0 the moment 50000000 is inserted?
;Your puzzle input is still 359.

; based on hint from https://www.reddit.com/r/adventofcode/comments/7kc0xw/2017_day_17_solutions/drd5y54/
(defn compute-part2 [step-num]
  (loop [count 1
         position 0
         val -1]
    (if (= count 50000000)
        val
        (let [pos (inc (mod (+ position step-num) count))]
          (if (= 1 pos)
              (recur (inc count) pos count)
              (recur (inc count) pos val))))))

(compute-part2 359)
;=> 39479736