(ns advent-of-code.day16
  (gen-class))

;--- Day 16: Permutation Promenade ---
;You come upon a very unusual sight; a group of programs here appear to be dancing.
;There are sixteen programs in total, named a through p. They start by standing in a line: a stands in position 0, b stands in position 1, and so on until p, which stands in position 15.
(def test-input [\a \b \c \d \e])
(def input [\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p])

;The programs' dance consists of a sequence of dance moves:
;
;Spin, written sX, makes X programs move from the end to the front, but maintain their order otherwise. (For example, s3 on abcde produces cdeab).
(defn spin [n coll]
  (vec
    (concat
      (take-last n coll)
      (take (- (count coll) n) coll))))

;Exchange, written xA/B, makes the programs at positions A and B swap places.
(defn exchange [posA posB coll]
  (assoc coll posA (get coll posB) posB (get coll posA)))

;Partner, written pA/B, makes the programs named A and B swap places.
(defn partner [a b coll]
  (exchange (.indexOf coll a) (.indexOf coll b) coll))
;For example, with only five programs standing in a line (abcde), they could do the following dance:
;
;s1, a spin of size 1: eabcd.
(spin 1 test-input)
;=> [\e \a \b \c \d]

;x3/4, swapping the last two programs: eabdc.
(->> test-input
     (spin 1)
     (exchange 3 4))
;=> [\e \a \b \d \c]

;pe/b, swapping programs e and b: baedc.
(->> test-input
     (spin 1)
     (exchange 3 4)
     (partner \e \b))
;=> [\b \a \e \d \c]
;After finishing their dance, the programs end up in order baedc.
;You watch the dance for a while and record their dance moves (your puzzle input). In what order are the programs standing after their dance?
(defn parse-command [c-str]
  (let [c (.charAt c-str 0)]
       (cond
         (= c \s) (partial spin (Integer/parseInt (second (re-matches #"s(\d+)" c-str))))
         (= c \x) (let [parsed (re-matches #"x(\d+)/(\d+)" c-str)]
                       (partial exchange (Integer/parseInt (get parsed 1)) (Integer/parseInt (get parsed 2))))
         (= c \p) (let [parsed (re-matches #"p(\S+)/(\S+)" c-str)]
                       (partial partner (.charAt ^String (get parsed 1) 0) (.charAt ^String (get parsed 2) 0))))))

(defn parse-commands [input]
  (->> (clojure.string/split input #",")
       (map #(clojure.string/trim %))
       (map #(parse-command %))))

(def m-dance
  (memoize
    (fn [commands coll]
      (reduce (fn [step command] (command step)) coll commands))))

(defn dance-times [input coll times]
  (let [commands (parse-commands input)]
    (loop [c times
           pos coll]
      (if (= 0 (mod c 100000)) (println c))
      (if (= c 0)
        pos
        (recur (dec c) (m-dance commands pos))))))

(apply str (dance-times "s1,x3/4,pe/b" test-input 1))
;=> "baedc"
(apply str (dance-times (slurp "src/advent_of_code/day16.in") input 1))
;=> "iabmedjhclofgknp"

;--- Part Two ---
;Now that you're starting to get a feel for the dance moves, you turn your attention to the dance as a whole.
;Keeping the positions they ended up in from their previous dance, the programs perform it again and again: including the first dance, a total of one billion (1000000000) times.
;
;In the example above, their second dance would begin with the order baedc, and use the same dance moves:
;
;s1, a spin of size 1: cbaed.
;x3/4, swapping the last two programs: cbade.
;pe/b, swapping programs e and b: ceadb.
;In what order are the programs standing after their billion dances?

(apply str (dance-times "s1,x3/4,pe/b" test-input 2))
;=> "ceadb"
(apply str (dance-times (slurp "src/advent_of_code/day16.in") input 10))
;=> "oifdcmleabhjnpkg"
(apply str (dance-times (slurp "src/advent_of_code/day16.in") input 100))
;=> "oifdcmleabhjnpkg"
(apply str (dance-times (slurp "src/advent_of_code/day16.in") input 1000))
;=> "oifdcmleabhjnpkg"
(apply str (dance-times (slurp "src/advent_of_code/day16.in") input 10000))
;=> "oifdcmleabhjnpkg"
; THEN MUST BE
;(apply str (dance-times (slurp "src/advent_of_code/day16.in") input 1000000000))
;=> "oifdcmleabhjnpkg"