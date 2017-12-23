(ns advent-of-code.day23
  (gen-class))

;--- Day 23: Coprocessor Conflagration ---
;You decide to head directly to the CPU and fix the printer from there. As you get close, you find an experimental coprocessor doing so much work that the local programs are afraid it will halt and catch fire. This would cause serious issues for the rest of the computer, so you head in and see what you can do.
;
;The code it's running seems to be a variant of the kind you saw recently on that tablet. The general functionality seems very similar, but some of the instructions are different:
;
;set X Y sets register X to the value of Y.
(defn reg-set [target value registers]
  (assoc registers target value))
;sub X Y decreases register X by the value of Y.
(defn reg-sub [target val registers]
  (assoc registers target (- (get registers target) val)))
;mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
(defn reg-mul [target val registers]
  (assoc registers target (* val (get registers target))))
;jnz X Y jumps with an offset of the value of Y, but only if the value of X is not zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
(defn reg-jnz [target offset registers]
  (if (not= (get registers target) 0)
    offset
    1)) ; move to next instruction
;Only the instructions listed above are used. The eight registers here, named a through h, all start at 0.
(defn position [c]
  (- (int c) (int \a)))

(defn create-register []
  (vec (repeat (inc (position \h)) 0)))
;The coprocessor is currently set to some kind of debug mode, which allows for testing, but prevents it from doing any meaningful work.
;
;If you run the program (your puzzle input), how many times is the mul instruction invoked?
(defn is-num? [str]
  (some? (re-matches #"-*\d+" str)))

(defn value-of [target registers]
  (if (nil? target)
    0
    (if (number? target)
      target
      (if (is-num? target)
        (Integer/parseInt target)
        (get registers (position (.charAt target 0)))))))

(defn do-instruction [ins-seq pos registers]
  (let [cmd-str (nth ins-seq pos)
        command (nth cmd-str 0)
        target-str (nth cmd-str 1)
        target (if (is-num? target-str) (Integer/parseInt target-str) (position (.charAt ^String target-str 0)))
        value (value-of (nth cmd-str 2) registers)
        new-pos (inc pos)]
    (cond
      (= command "set") [new-pos (reg-set target value registers) :set]
      (= command "sub") [new-pos (reg-sub target value registers) :sub]
      (= command "mul") [new-pos (reg-mul target value registers) :mul]
      (= command "jnz") [(+ pos (reg-jnz target value registers)) registers :jnz]
      :else (println ins-seq "not supported!"))))

(use 'advent-of-code.utils)

(defn parse-seq [input]
  (->>
    (to-lines input)
    (map #(re-matches #"^([a-z]{3})\s(\d+|[a-z])(?>\s(-*\d+|[a-z]))*$" %))
    (filter some?)
    (map #(subvec % 1))))

(defn process-instruction-count-mul [input]
  (loop [s (parse-seq input)
         p 0
         cm 0
         r (create-register)]
    (if (>= p (count s))
      cm
      (let [[new-p new-r last-cmd] (do-instruction s p r)]
        (if (= last-cmd :mul)
          (recur s new-p (inc cm) new-r)
          (recur s new-p cm new-r))))))

(process-instruction-count-mul (slurp "src/advent_of_code/day23.in"))
;=> 3025

;--- Part Two ---
;Now, it's time to fix the problem.
;The debug mode switch is wired directly to register a. You flip the switch, which makes register a now start at 1 when the program is executed.
;Immediately, the coprocessor begins to overheat. Whoever wrote this program obviously didn't choose a very efficient implementation. You'll need to optimize the program if it has any hope of completing before Santa needs that printer working.
;The coprocessor's ultimate goal is to determine the final value left in register h once the program completes. Technically, if it had that... it wouldn't even need to run the program.
;After setting register a to 1, if the program were to run to completion, what value would be left in register h?

; based on https://www.reddit.com/r/adventofcode/comments/7lms6p/2017_day_23_solutions/drnl3gg/
; is-prime from https://github.com/fardog/is-prime/blob/master/src/is_prime/core.clj
(defn test-prime [x]
  (loop [iter 2 top (int (Math/sqrt x))]
    (if (> iter top)
      true
      (if (= 0 (mod x iter))
        false
        (recur (inc iter) top)))))

(defn is-prime [x]
  (if (< x 2)
    false
    (test-prime x)))

(->> (range 105700 122701 17)
     (filter #(not (is-prime %)))
     (count))
