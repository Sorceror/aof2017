;You sit for a while and record part of the stream (your puzzle input). The characters represent groups - sequences that begin with { and end with }. Within a group, there are zero or more other things, separated by commas: either another group or garbage. Since groups can contain other groups, a } only closes the most-recently-opened unclosed group - that is, they are nestable. Your puzzle input represents a single, large group which itself contains many smaller ones.
;Sometimes, instead of a group, you will find garbage. Garbage begins with < and ends with >. Between those angle brackets, almost any character can appear, including { and }. Within garbage, < has no special meaning.
;In a futile attempt to clean up the garbage, some program has canceled some of the characters within it using !: inside garbage, any character that comes after ! should be ignored, including <, >, and even another !.
;You don't see any characters that deviate from these rules. Outside garbage, you only find well-formed groups, and garbage always terminates according to the rules above.
;
;Your goal is to find the total score for all groups in your input. Each group is assigned a score which is one more than the score of the group that immediately contains it. (The outermost group gets a score of 1.)
;What is the total score for all groups in your input?

(ns advent-of-code.day9
  (gen-class))

(def input "<{!>}>")
(def input "<>")
(def input "<random characters>")
(def input "<!!>")
(def input "<!!!>>")
(def input "<{o\"i!a,<{i<a>")
(def input "{{<!>},{<!>},{<!>},{<a>}}")
(def input "{}")
(def input "{{{}}}")
(def input "{{},{}}")
(def input "{{{},{},{{}}}}")
(def input "{<{},{},{{}}>}")
(def input "{<a>,<a>,<a>,<a>}")
(def input "{{<a>},{<a>},{<a>},{<a>}}")
(def input "{{<!>},{<!>},{<!>},{<a>}}")
(def input "{{<ab>},{<ab>},{<ab>},{<ab>}}")

(defn count-groups [s]
  (loop [step 0
         to-see s
         counter 0]
    (if (empty? to-see)
        counter
        (let [curr-char (first to-see)]
          (cond
            (= curr-char \{) (recur (inc step) (drop 1 to-see) (+ counter (inc step)))
            (= curr-char \}) (recur (dec step) (drop 1 to-see) counter)
            :else (recur step (drop 1 to-see) counter))))))

(defn parse [s]
  (-> s
    (clojure.string/replace #"\!." "")
    (clojure.string/replace #"\<[^\>]*\>" "")
    (count-groups)))

(parse input)
(parse (slurp "src/advent_of_code/day9.in"))

; ---- PART TWO ----
;Now, you're ready to remove the garbage.
;To prove you've removed it, you need to count all of the characters within the garbage. The leading and trailing < and > don't count, nor do any canceled characters or the ! doing the canceling.
;How many non-canceled characters are within the garbage in your puzzle input?

(defn count-garbage [s]
  (->> s
    (re-seq #"\<([^\>]*)\>")
    (map #(second %))
    (map #(count %))
    (reduce +)))

(defn parse2 [s]
  (-> s
    (clojure.string/replace #"\!." "")
    (count-garbage)))

(parse2 input)
(parse2 (slurp "src/advent_of_code/day9.in"))