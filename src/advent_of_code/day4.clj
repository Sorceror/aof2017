; To ensure security, a valid passphrase must contain no duplicate words.
;
; For example:
;
; aa bb cc dd ee is valid.
; aa bb cc dd aa is not valid - the word aa appears more than once.
; aa bb cc dd aaa is valid - aa and aaa count as different words.

(def input "aa bb cc dd ee
            bb ee dd
            bb bb bb")

(defn parse [input]
  (->>
    (clojure.string/split-lines input)
    (map #(clojure.string/trim %))))

(defn to-words [input]
  (clojure.string/split input #"\s"))

(defn valid-passphrase? [passphrase]
  (as-> (to-words passphrase) v
    (= (count v) (count (distinct v)))))

(defn count-valid [input f]
  (->>
    (parse input)
    (map #(f %))
    (filter #(= true %))
    (count)))

(count-valid (slurp "src/advent_of_code/day4.in") valid-passphrase?)

; --- PART TWO ---
; For added security, yet another system policy has been put in place. Now, a valid passphrase must contain no two words that are anagrams of each other - that is, a passphrase is invalid if any word's letters can be rearranged to form any other word in the passphrase.
;
; For example:
;
; abcde fghij is a valid passphrase.
; abcde xyz ecdab is not valid - the letters from the third word can be rearranged to form the first word.
; a ab abc abd abf abj is a valid passphrase, because all letters need to be used when forming another word.
; iiii oiii ooii oooi oooo is valid.
; oiii ioii iioi iiio is not valid - any of these words can be rearranged to form any other word.

; (def input "abcde fghij")
; (def input "oiii ioii iioi iiio")

(defn valid-passphrase2? [passphrase]
  (as-> (to-words passphrase) v
    (map #(frequencies (vec %)) v)
    (= (count v) (count (distinct v)))))

(count-valid (slurp "src/advent_of_code/day4.in") valid-passphrase2?)
