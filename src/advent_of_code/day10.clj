;To achieve this, begin with a list of numbers from 0 to 255, a current position which begins at 0 (the first element in the list), a skip size (which starts at 0), and a sequence of lengths (your puzzle input). Then, for each length:
;Reverse the order of that length of elements in the list, starting with the element at the current position.
;Move the current position forward by that length plus the skip size.
;Increase the skip size by one.
;The list is circular; if the current position and the length try to reverse elements beyond the end of the list, the operation reverses using as many extra elements as it needs from the front of the list. If the current position moves past the end of the list, it wraps around to the front. Lengths larger than the size of the list are invalid.
;However, you should instead use the standard list size of 256 (with values 0 to 255) and the sequence of lengths in your puzzle input. Once this process is complete, what is the result of multiplying the first two numbers in the list?

(ns advent-of-code.day10
  (gen-class))

(defn create-list [size]
  (vec (range size)))

(defn get-ind [curr-pos list]
  (mod curr-pos (count list)))

(defn get-pos [curr-pos list]
  (get list (get-ind curr-pos list)))

(defn get-part [curr-pos length list]
  (loop [p curr-pos
         l length
         part []]
    (if (= l 0)
        part
        (recur (inc p) (dec l) (conj part (get-pos p list))))))

(defn set-part [curr-pos list part]
  (loop [p curr-pos
         to-add part
         new-list list]
    (if (empty? to-add)
        new-list
        (recur (inc p) (drop 1 to-add) (assoc new-list (get-ind p list) (first to-add))))))

(defn reverse-part [curr-pos length list]
  (->> list
    (get-part curr-pos length)
    (reverse)
    (vec)
    (set-part curr-pos list)))

(defn compute-hash [list lengths]
  (loop [p 0
         ss 0
         ls lengths
         l list]
    (if (empty? ls)
        (* (first l) (second l))
        (recur
          (+ p ss (first ls))
          (inc ss)
          (drop 1 ls)
          (reverse-part p (first ls) l)))))

(compute-hash (create-list 5) [3 4 1 5])
(compute-hash (create-list 256) [97 167 54 178 2 11 209 174 119 248 254 0 255 1 64 190])

; ---- PART TWO ----
; First, from now on, your input should be taken not as a list of numbers, but as a string of bytes instead. Unless otherwise specified, convert characters to bytes using their ASCII codes. This will allow you to handle arbitrary ASCII strings, and it also ensures that your input lengths are never larger than 255. For example, if you are given 1,2,3, you should convert it to the ASCII codes for each character: 49,44,50,44,51.
;Once you have determined the sequence of lengths to use, add the following lengths to the end of the sequence: 17, 31, 73, 47, 23. For example, if you are given 1,2,3, your final sequence of lengths should be 49,44,50,44,51,17,31,73,47,23 (the ASCII codes from the input string combined with the standard length suffix values).
;Second, instead of merely running one round like you did above, run a total of 64 rounds, using the same length sequence in each round. The current position and skip size should be preserved between rounds. For example, if the previous example was your first round, you would start your second round with the same length sequence (3, 4, 1, 5, 17, 31, 73, 47, 23, now assuming they came from ASCII codes and include the suffix), but start with the previous round's current position (4) and skip size (4).
;Once the rounds are complete, you will be left with the numbers from 0 to 255 in some order, called the sparse hash. Your next task is to reduce these to a list of only 16 numbers called the dense hash. To do this, use numeric bitwise XOR to combine each consecutive block of 16 numbers in the sparse hash (there are 16 such blocks in a list of 256 numbers). So, the first element in the dense hash is the first sixteen elements of the sparse hash XOR'd together, the second element in the dense hash is the second sixteen elements of the sparse hash XOR'd together, etc.
;Perform this operation on each of the sixteen blocks of sixteen numbers in your sparse hash to determine the sixteen numbers in your dense hash.
;Finally, the standard way to represent a Knot Hash is as a single hexadecimal string; the final output is the dense hash in hexadecimal notation. Because each number in your dense hash will be between 0 and 255 (inclusive), always represent each number as two hexadecimal digits (including a leading zero as necessary). So, if your first three numbers are 64, 7, 255, they correspond to the hexadecimal numbers 40, 07, ff, and so the first six characters of the hash would be 4007ff. Because every Knot Hash is sixteen such numbers, the hexadecimal representation is always 32 hexadecimal digits (0-f) long.
;Treating your puzzle input as a string of ASCII characters, what is the Knot Hash of your puzzle input? Ignore any leading or trailing whitespace you might encounter.

(defn to-ASCII [s]
  (vec (map int s)))

(defn append-suffix [v]
  (vec (concat v [17 31 73 47 23])))

(defn compute-hash2 [list lengths]
  (loop [p 0
         ss 0
         ls lengths
         l list
         c 64]
    (if (= c 0)
        l
        (if (empty? ls)
          (recur
            (get-ind p list) ss lengths l (dec c))
          (recur
            (get-ind (+ p ss (first ls)) list)
            (inc ss)
            (drop 1 ls)
            (reverse-part p (first ls) l)
            c)))))

(defn split-to-blocks [num v]
  (loop [c v
         res []]
    (if (empty? c)
        res
        (recur (drop num c) (conj res (vec (take num c)))))))

(defn dense-block [v]
  (apply bit-xor v))

(defn to-hex-str [v]
  (apply str
         (map #(format "%02x" %) v)))

(defn dense-hash [s]
  (->> s
       (to-ASCII)
       (append-suffix)
       (compute-hash2 (create-list 256))
       (split-to-blocks 16)
       (map #(dense-block %))
       (to-hex-str)))

(dense-hash "")
(dense-hash "AoC 2017")
(dense-hash "1,2,3")
(dense-hash "1,2,4")
(dense-hash "97,167,54,178,2,11,209,174,119,248,254,0,255,1,64,190")