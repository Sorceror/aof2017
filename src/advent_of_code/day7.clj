(ns advent-of-code.day7
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

(find-root (parse-nodes input))
(find-root (parse-nodes (slurp "src/advent_of_code/day7.in")))

;  ---- PART TWO ----
; (defn get-node-weigth [node nodes]
;   (get-in nodes [node :weight]))
;
; (defn get-node-children [node nodes]
;   (get-in nodes [node :children]))
;
; (defn all-children-of [node nodes]
;   (let [children (get-node-children node nodes)]
;        (if (not-empty children)
;            (reduce (fn [coll child]
;                        (union coll #{child} (all-children-of child nodes)))
;                    #{} children)
;            #{})))
;
;
; (defn sum-children [node nodes]
;   (reduce (fn [total child]
;               (+ total))
;           0 (all-children-of node nodes)))
;
; (defn sums-equal? [sums]
;   (apply = (map (fn [[node children-sum weight total]] total) sums)))
;
; (trace-vars sums-equal?)
; (trace-special-form loop)
;
; (defn find-unbalance [root nodes]
;   (loop [to-visit [root]]
;     (println to-visit)
;     (let [node (first to-visit)
;           children (get-node-children root nodes)
;           children-sums (map (fn [node]
;                                  [node (sum-children node nodes) (get-node-weigth node nodes) (+ (sum-children node nodes) (get-node-weigth node nodes))])
;                              children)]
;          (if (sums-equal? children-sums)
;              (recur (conj (drop 1 to-visit) children))
;              children-sums))))
;
;        ; children-sums))
;
; (find-unbalance :tknk (parse-nodes input))
;
; (let [nodes (parse-nodes (slurp "src/advent_of_code/day7.in"))
;       root (find-root nodes)]
;   (find-unbalance (first root) nodes))
