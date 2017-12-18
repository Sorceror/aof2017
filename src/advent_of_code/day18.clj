(ns advent-of-code.day18
  (gen-class))

;--- Day 18: Duet ---
;You discover a tablet containing some strange assembly code labeled simply "Duet". Rather than bother the sound card with it, you decide to run the code yourself. Unfortunately, you don't see any documentation, so you're left to figure out what the instructions mean on your own.
;It seems like the assembly is meant to operate on a set of registers that are each named with a single letter and that can each hold a single integer. You suppose each register should start with a value of 0.
(defn position [c]
  (- (int c) (int \a)))

(defn create-register []
  (vec (repeat (inc (position \z)) 0)))
;There aren't that many instructions, so it shouldn't be hard to figure out what they do. Here's what you determine:
;
;snd X plays a sound with a frequency equal to the value of X.
;(defn reg-snd [val atom]
;  (swap! atom (fn [_] val)))
;set X Y sets register X to the value of Y.
(defn reg-set [target value registers]
  (assoc registers target value))
;add X Y increases register X by the value of Y.
(defn reg-add [target val registers]
  (assoc registers target (+ val (get registers target))))
;mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
(defn reg-mul [target val registers]
  (assoc registers target (* val (get registers target))))
;mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it sets X to the result of X modulo Y).
(defn reg-mod [target val registers]
  (assoc registers target (mod (get registers target) val)))
;rcv X recovers the frequency of the last sound played, but only when the value of X is not zero. (If it is zero, the command does nothing.)
(defn reg-rcv [target last-sound registers]
  (if (not= (get registers target) 0)
      (assoc registers target last-sound)
      registers))
;jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
(defn reg-jgz [target offset registers]
  (if (> (get registers target) 0)
      offset
      1)) ; move to next instruction

;Many of the instructions can take either a register (a single letter) or a number. The value of a register is the integer it contains; the value of a number is that number.
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

;After each jump instruction, the program continues with the instruction to which the jump jumped. After any other instruction, the program continues with the next instruction. Continuing (or jumping) off either end of the program terminates it.
(defn do-instruction [ins-seq pos last-sound registers]
  (let [cmd-str (nth ins-seq pos)
        command (nth cmd-str 0)
        target (position (.charAt ^String (nth cmd-str 1) 0))
        value (value-of (nth cmd-str 2) registers)
        new-pos (inc pos)]
    (cond
      (= command "snd") [new-pos (get registers (value-of target registers) registers) registers :snd]
      (= command "set") [new-pos last-sound (reg-set target value registers) :set]
      (= command "add") [new-pos last-sound (reg-add target value registers) :add]
      (= command "mul") [new-pos last-sound (reg-mul target value registers) :mul]
      (= command "mod") [new-pos last-sound (reg-mod target value registers) :mod]
      (= command "rcv") [new-pos last-sound (reg-rcv target last-sound registers) :rcv]
      (= command "jgz") [(+ pos (reg-jgz target value registers)) last-sound registers :jgz]
      :else (println ins-seq "not supported!"))))

(def test-sequence
  "set a 1
   add a 2
   mul a a
   mod a 5
   snd a
   set a 0
   rcv a
   jgz a -1
   set a 1
   jgz a -2")

(use 'advent-of-code.utils)

(defn parse-seq [input]
  (->>
      (to-lines input)
      (map #(re-matches #"([a-z]{3})\s([a-z])(?>\s(-*\d+|[a-z]))*" %))
      (filter some?)
      (map #(subvec % 1))))

(defn process-instruction-until-rcv [input]
  (loop [s (parse-seq input)
         p 0
         ls 0
         r (create-register)]
    (if (>= p (count s))
        ls
        (let [[new-p new-ls new-r last-cmd] (do-instruction s p ls r)]
          (if (and (not= 0 new-ls) (= last-cmd :rcv))
              new-ls
              (recur s new-p new-ls new-r))))))

(process-instruction-until-rcv test-sequence)
;=> 4
(process-instruction-until-rcv (slurp "src/advent_of_code/day18.in"))
;=> 3423

;--- Part Two ---
;As you congratulate yourself for a job well done, you notice that the documentation has been on the back of the tablet this entire time. While you actually got most of the instructions correct, there are a few key differences. This assembly code isn't about sound at all - it's meant to be run twice at the same time.
;Each running copy of the program has its own set of registers and follows the code independently - in fact, the programs don't even necessarily run at the same speed. To coordinate, they use the send (snd) and receive (rcv) instructions:
;snd X sends the value of X to the other program. These values wait in a queue until that program is ready to receive them. Each program has its own message queue, so a program can never receive a message it sent.
(defn reg-snd2 [target programID queues register]
  (assoc queues programID (conj (get queues programID) (value-of target register))))
;rcv X receives the next value and stores it in register X. If no values are in the queue, the program waits for a value to be sent to it. Programs do not continue to the next instruction until they have received a value. Values are received in the order they are sent.
(defn reg-rcv2 [target programID queues register])

;Each program also has its own program ID (one 0 and the other 1); the register p should begin with this value.
(defn create-registers []
  [(-> (create-register)
       (assoc (position \p) 0))
   (-> (create-register)
       (assoc (position \p) 1))])
;
;For example:
;
;snd 1
;snd 2
;snd p
;rcv a
;rcv b
;rcv c
;rcv d
;Both programs begin by sending three values to the other. Program 0 sends 1, 2, 0; program 1 sends 1, 2, 1. Then, each program receives a value (both 1) and stores it in a, receives another value (both 2) and stores it in b, and then each receives the program ID of the other program (program 0 receives 1; program 1 receives 0) and stores it in c. Each program now sees a different value in its own copy of register c.
;Finally, both programs try to rcv a fourth time, but no data is waiting for either of them, and they reach a deadlock. When this happens, both programs terminate.
;It should be noted that it would be equally valid for the programs to run at different speeds; for example, program 0 might have sent all three values and then stopped at the first rcv before program 1 executed even its first instruction.
;Once both of your programs have terminated (regardless of what caused them to do so), how many times did program 1 send a value?
(defn do-instruction [programID ins-seq pos registers queues]
  (let [cmd-str (nth ins-seq pos)
        command (nth cmd-str 0)
        target (position (.charAt ^String (nth cmd-str 1) 0))
        value (value-of (nth cmd-str 2) registers)
        new-pos (inc pos)]
    (cond
      (= command "snd") [new-pos registers (assoc queues programID (conj (get queues programID) (value-of target registers)))]
      (= command "set") [new-pos (reg-set target value registers) queues]
      (= command "add") [new-pos (reg-add target value registers) queues]
      (= command "mul") [new-pos (reg-mul target value registers) queues]
      (= command "mod") [new-pos (reg-mod target value registers) queues]
      (= command "rcv") [new-pos (reg-rcv target last-sound registers) queues]
      (= command "jgz") [(+ pos (reg-jgz target value registers)) last-sound registers]
      :else (println ins-seq "not supported!"))))