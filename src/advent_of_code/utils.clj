(ns advent-of-code.utils
  (:gen-class))

(defn to-digit [s]
  (Integer/parseInt s))

(defn to-digits [row]
  (map #(to-digit %)
    row))

(defn to-lines [s]
  (->>
    (clojure.string/split-lines s)
    (map #(clojure.string/trim %))))

(defn parse [s]
  (->>
    (to-lines)
    (map #(clojure.string/split % #"\s"))
    (map #(to-digits %))
    (flatten)
    (vec)))

(defn to-binary-string [val size]
  (as-> (Integer/toBinaryString val) v
        (format (str "%1$" size "s") v)
        (clojure.string/replace v " " "0")))