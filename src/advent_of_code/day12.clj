(ns advent-of-code.day12
  (gen-class))

;--- Day 12: Digital Plumber ---
;Walking along the memory banks of the stream, you find a small village that is experiencing a little confusion: some programs can't communicate with each other.
;Programs in this village communicate using a fixed system of pipes. Messages are passed between programs using these pipes, but most programs aren't connected to each other directly. Instead, programs pass messages between each other until the message reaches the intended recipient.
;For some reason, though, some of these messages aren't ever reaching their intended recipient, and the programs suspect that some pipes are missing. They would like you to investigate.
;You walk through the village and record the ID of each program and the IDs with which it can communicate directly (your puzzle input). Each program has one or more programs with which it can communicate, and these pipes are bidirectional; if 8 says it can communicate with 11, then 11 will say it can communicate with 8.
;You need to figure out how many programs are in the group that contains program ID 0.
;For example, suppose you go door-to-door like a travelling salesman and record the following list:
;
;0 <-> 2
;1 <-> 1
;2 <-> 0, 3, 4
;3 <-> 2, 4
;4 <-> 2, 3, 6
;5 <-> 6
;6 <-> 4, 5

(use 'advent-of-code.utils)
(use 'clojure.set)

(def test-input
  "0 <-> 2
  1 <-> 1
  2 <-> 0, 3, 4
  3 <-> 2, 4
  4 <-> 2, 3, 6
  5 <-> 6
  6 <-> 4, 5")

(defn to-edge-list [vertices]
  (loop [root (first vertices)
         rest (drop 1 vertices)
         edge-list '()]
    (let [node (first rest)]
         (if (empty? rest)
             edge-list
             (recur root (drop 1 rest) (conj edge-list (list root node) (list node root)))))))

(defn parse-node [str]
  (->> str
       (re-seq #"\d+")
       (map #(to-digit %))
       (to-edge-list)))

(defn parse-nodes [strs]
  (->> strs
       (to-lines)
       (reduce (fn [coll item] (concat coll (parse-node item))) '())))

;In this example, the following programs are in the group that contains program ID 0:
;
;Program 0 by definition.
;Program 2, directly connected to program 0.
;Program 3 via program 2.
;Program 4 via program 2.
;Program 5 via programs 6, then 4, then 2.
;Program 6 via programs 4, then 2.
;Therefore, a total of 6 programs are in this group; all but program 1, which has a pipe that connects it to itself.

(def m-all-vertices
  (memoize
    (fn [edge-list]
      (reduce (fn [coll item] (conj coll (first item))) #{} edge-list))))

(def m-other-vertices
  (memoize
    (fn [v edge-list]
        (-> edge-list
            (m-all-vertices)
            (disj v)))))

(def m-neighbours
  (memoize
    (fn [v edge-list]
      (->>
        (filter #(= (second %) v) edge-list)
        (m-other-vertices v)))))

(def m-path?
  (memoize
    (fn [v1 v2 edge-list]
      (loop [to-see #{v1}
             seen #{}]
        (if (empty? to-see)
            false
            (let [node (first to-see)
                  ns (m-neighbours node edge-list)]
              (if (contains? ns v2)
                  true
                  (recur (disj (union (difference ns seen) to-see) node) (conj seen node)))))))))

;How many programs are in the group that contains program ID 0?

(defn count-with-path-to [v edge-list]
  (->> edge-list
       (m-other-vertices v)
       (map #(m-path? v % edge-list))
       (filter true?)
       (count)
       (inc))) ; because by definition there is always at the least path to itself

(count-with-path-to 0 (parse-nodes test-input))
(count-with-path-to 1 (parse-nodes test-input))
(count-with-path-to 0 (parse-nodes (slurp "src/advent_of_code/day12.in")))

; --- Part Two ---
;There are more programs than just the ones in the group containing program ID 0. The rest of them have no way of reaching that group, and still might have no way of reaching each other.
;A group is a collection of programs that can all communicate via pipes either directly or indirectly. The programs you identified just a moment ago are all part of the same group. Now, they would like you to determine the total number of groups.
;In the example above, there were 2 groups: one consisting of programs 0,2,3,4,5,6, and the other consisting solely of program 1.

;How many groups are there in total?

(defn group-for [v edge-list other-groups]
  (->>
      (difference (m-other-vertices v edge-list) other-groups)
      (reduce (fn [coll item]
                  (if (m-path? v item edge-list)
                      (conj coll item)
                      coll))
              #{})
      (union #{v})))

(defn count-groups [edge-list]
  (loop [to-see (m-all-vertices edge-list)
         c 0
         seen #{}]
    (println (count to-see))
    (if (empty? to-see)
        c
        (let [node (first to-see)
              group (group-for node edge-list seen)]
          (recur (difference to-see group) (inc c) (union group seen))))))

(count-groups (parse-nodes test-input))
(count-groups (parse-nodes (slurp "src/advent_of_code/day12.in")))