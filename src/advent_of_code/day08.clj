;Each instruction consists of several parts: the register to modify, whether to increase or decrease that register's value, the amount by which to increase or decrease it, and a condition. If the condition fails, skip the instruction without modifying the register. The registers all start at 0. The instructions look like this:
;
;b inc 5 if a > 1
;a inc 1 if b < 5
;c dec -10 if a >= 1
;c inc -20 if c == 10
;These instructions would be processed as follows:
;
;Because a starts at 0, it is not greater than 1, and so b is not modified.
;a is increased by 1 (to 1) because b is less than 5 (it is 0).
;c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is 1).
;c is increased by -20 (to -10) because c is equal to 10.
;After this process, the largest value in any register is 1.
;
;You might also encounter <= (less than or equal to) or != (not equal to). However, the CPU doesn't have the bandwidth to tell you what all the registers are named, and leaves that to you to determine.
;
;What is the largest value in any register after completing the instructions in your puzzle input?

(ns advent-of-code.day08
  (gen-class))

(use 'advent-of-code.utils)

(def input
  "b inc 5 if a > 1
   a inc 1 if b < 5
   c dec -10 if a >= 1
   c inc -20 if c == 10")

(defn command-to-map [v]
  {:target        (get v 0)
   :target-action (if (= "inc" (get v 1)) "+" "-")
   :action-value  (get v 2)
   :cond-target   (get v 3)
   :cond-func     (let [fn-name (get v 4)]
                       (cond
                         (= fn-name "!=") "not="
                         :else fn-name))
   :cond-value    (get v 5)})

(defn parse-command [s]
  (->> s
    (re-matches #"([a-z]+)\s+([inc|dec]{3})\s+([+-]?\d+)\s+if\s+([a-z]+)\s+([><=!]{1,2})\s+([+-]?\d+)")
    (drop 1)
    (vec)
    (command-to-map)))

(defn get-register [r rs]
  (if (contains? rs r)
      (get rs r)
      0))

(defn str-fn [s]
  (resolve (symbol s)))

(defn eval-command [c rs]
  (let [cond-fn (str-fn (get c :cond-func))
        cond-lv (get-register (symbol (get c :cond-target)) rs)
        cond-rv (to-digit (get c :cond-value))
        tar-fn (str-fn (get c :target-action))
        tar-chg (to-digit (get c :action-value))
        tar-key (symbol (get c :target))
        curr-v (get-register tar-key rs)]
    (if (cond-fn cond-lv cond-rv)
        (assoc rs tar-key (tar-fn curr-v tar-chg))
        (assoc rs tar-key curr-v))))

(defn call-commands [cs]
  (loop [commands cs
         registers {}]
    (if (empty? commands)
        registers
        (recur
          (drop 1 commands)
          (eval-command (first commands) registers)))))

(defn find-max-register [rs]
  (if (empty? rs)
      [0 0]
      (apply max-key val rs)))

(defn max-register [input]
  (->> input
    (to-lines)
    (map #(parse-command %))
    (call-commands)
    (find-max-register)))

(max-register input)
(max-register (slurp "src/advent_of_code/day08.in"))

; ---- PART TWO ----
; To be safe, the CPU also needs to know the highest value held in any register during this process so that it can decide how much memory to allocate to these operations. For example, in the above instructions, the highest value ever held was 10 (in register c after the third instruction was evaluated).

(defn call-commands2 [cs]
  (loop [commands cs
         registers {}
         highest-so-far 0]
    (let [highest-reg (second (find-max-register registers))]
      (if (empty? commands)
        highest-so-far
        (recur
          (drop 1 commands)
          (eval-command (first commands) registers)
          (max highest-reg highest-so-far))))))

(defn max-register2 [input]
  (->> input
       (to-lines)
       (map #(parse-command %))
       (call-commands2)))

(max-register2 input)
(max-register2 (slurp "src/advent_of_code/day08.in"))
