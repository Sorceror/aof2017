(ns advent-of-code.utils
  (:gen-class))

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
