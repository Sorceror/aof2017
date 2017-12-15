;One program at the bottom supports the entire tower. It's holding a large disc, and on the disc are balanced several more sub-towers. At the bottom of these sub-towers, standing on the bottom disc, are other programs, each holding their own disc, and so on. At the very tops of these sub-sub-sub-...-towers, many programs stand simply keeping the disc below them balanced but with no disc of their own.
;You offer to help, but first you need to understand the structure of these towers. You ask each program to yell out their name, their weight, and (if they're holding a disc) the names of the programs immediately above them balancing on that disc. You write this information down (your puzzle input). Unfortunately, in their panic, they don't do this in an orderly fashion; by the time you're done, you're not sure which program gave which information.
;Before you're ready to help them, you need to make sure your information is correct. What is the name of the bottom program?

(ns advent-of-code.day07
  (gen-class))

(def input
  "pbga (66)
  xhth (57)
  ebii (61)
  havc (66)
  ktlj (57)
  fwft (72) -> ktlj, cntj, xhth
  qoyq (66)
  padx (45) -> pbga, havc, qoyq
  tknk (41) -> ugml, padx, fwft
  jptl (61)
  ugml (68) -> gyxo, ebii, jptl
  gyxo (61)
  cntj (57)")

(use 'advent-of-code.utils)
(use 'clojure.set)

(defn parse-line [s]
  (->>
    (re-matches #"(\S*)\s\((\d+)\)(?>\s->\s(.*))*" s)
    (drop 1)
    (vec)))

(defn parse-children [s]
  (if (some? s)
      (->>
        (clojure.string/split s #",")
        (map #(clojure.string/trim %))
        (map #(keyword %))
        (vec))
      []))

(defn parse-node [name weight children-str]
  {(keyword name) {:weight (to-digit weight)
                   :children (parse-children children-str)}})

(defn parse-nodes [input]
  (->>
    (to-lines input)
    (map #(parse-line %))
    (map #(apply parse-node %))
    (into {})))

(defn all-nodes-set [nodes]
  (->> nodes
    (map #(key %))
    (set)))

(defn all-children-set [nodes]
  (->> nodes
    (map #(get (val %) :children))
    (flatten)
    (set)))

(defn find-root [input]
  (difference (all-nodes-set input) (all-children-set input)))

(type (parse-nodes (slurp "src/advent_of_code/day07.in")))
(find-root (parse-nodes input))
(find-root (parse-nodes (slurp "src/advent_of_code/day07.in")))

;  ---- PART TWO ----
;For any program holding a disc, each program standing on that disc forms a sub-tower. Each of those sub-towers are supposed to be the same weight, or the disc itself isn't balanced. The weight of a tower is the sum of the weights of the programs in that tower.
;Given that exactly one program is the wrong weight, what would its weight need to be to balance the entire tower?

(defn get-node-weight [node nodes]
 (get-in nodes [node :weight]))

(defn get-node-children [node nodes]
  (get-in nodes [node :children]))

(defn all-children-of [node nodes]
  (let [children (get-node-children node nodes)]
       (if (not-empty children)
           (reduce (fn [coll child]
                       (union coll #{child} (all-children-of child nodes)))
                   #{} children)
           #{})))

(defn sum-children [node nodes]
  (reduce (fn [total child]
              (+ total (get-node-weight child nodes)))
          0 (all-children-of node nodes)))

(defn sums-equal? [sums]
  (if (empty? sums)
    true
    (apply = (map (fn [[_ _ _ total]] total) sums))))

(defn find-unbalance [root nodes]
  (loop [to-visit [root]
         unbalanced []]
    (if (empty? to-visit)
        unbalanced
        (let [node (first to-visit)
              children (get-node-children node nodes)
              children-sums (map (fn [node]
                                     [node (sum-children node nodes) (get-node-weight node nodes) (+ (sum-children node nodes) (get-node-weight node nodes))])
                                 children)]
             (if (sums-equal? children-sums)
                 (recur (concat (drop 1 to-visit) children) unbalanced)
                 (recur (concat (drop 1 to-visit) children) (conj unbalanced children-sums)))))))

(find-unbalance :tknk (parse-nodes input))

(let [nodes (parse-nodes (slurp "src/advent_of_code/day07.in"))
      root (find-root nodes)]
  (find-unbalance (first root) nodes))
