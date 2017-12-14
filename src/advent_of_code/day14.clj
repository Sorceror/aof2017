(ns advent-of-code.day14
  (gen-class))

;--- Day 14: Disk Defragmentation ---
;Suddenly, a scheduled job activates the system's disk defragmenter. Were the situation different, you might sit and watch it for a while, but today, you just don't have that kind of time. It's soaking up valuable system resources that are needed elsewhere, and so the only option is to help it finish its task as soon as possible.
;The disk in question consists of a 128x128 grid; each square of the grid is either free or used. On this disk, the state of the grid is tracked by the bits in a sequence of knot hashes.
;A total of 128 knot hashes are calculated, each corresponding to a single row in the grid; each hash contains 128 bits which correspond to individual grid squares. Each bit of a hash indicates whether that square is free (0) or used (1).
;The hash inputs are a key string (your puzzle input), a dash, and a number from 0 to 127 corresponding to the row. For example, if your key string were flqrgnkx, then the first row would be given by the bits of the knot hash of flqrgnkx-0, the second row from the bits of the knot hash of flqrgnkx-1, and so on until the last row, flqrgnkx-127.
(def test-input "a0c2017")

(defn hash-input-seq [input]
  (map #(str input "-" %) (range 128)))

(use 'advent-of-code.day10)

(defn hash-seq [input-seq]
  (map #(dense-hash %) input-seq))

;The output of a knot hash is traditionally represented by 32 hexadecimal digits; each of these digits correspond to 4 bits, for a total of 4 * 32 = 128 bits. To convert to bits, turn each hexadecimal digit to its equivalent binary value, high-bit first: 0 becomes 0000, 1 becomes 0001, e becomes 1110, f becomes 1111, and so on; a hash that begins with a0c2017... in hexadecimal would begin with 10100000110000100000000101110000... in binary.
(defn to-bit-seq [hash]
  (->>
    (partition 2 hash)
    (map (fn [[x y]] (Integer/parseInt (str x y) 16)))
    (map #(Integer/toBinaryString %))
    (map #(format "%1$8s" %))
    (map #(clojure.string/replace % " " "0"))
    (apply str)))

(defn count-used-in-single [b-seq]
  (reduce
    (fn [count item]
        (if (= \1 item)
            (inc count)
            count))
    0 b-seq))

(defn parse-disk [input]
  (->> input
       (hash-input-seq)
       (hash-seq)
       (map #(to-bit-seq %))
       (vec)))

(defn count-used [input]
  (->> (parse-disk input)
       (map #(count-used-in-single %))
       (reduce +)))

(count-used "flqrgnkx")
;=> 8108
(count-used "jxqlasbh")
;=> 8140

;--- Part Two ---
;Now, all the defragmenter needs to know is the number of regions. A region is a group of used squares that are all adjacent, not including diagonals. Every used square is in exactly one region: lone used squares form their own isolated regions, while several adjacent squares all count as a single region.
;Of particular interest is the region marked 8; while it does not appear contiguous in this small view, all of the squares marked 8 are connected when considering the whole 128x128 grid. In total, in this example, 1242 regions are present.
;How many regions are present given your key string?

(def dirs [[0 1] [1 0] [0 -1] [-1 0]])

(defn with-dir [c d]
  [(+ (first c) (first d)) (+ (second c) (second d))])

(defn is-occupied? [c disk]
  (let [x (first c)
        y (second c)]
    (cond
      (< x 0) false
      (< y 0) false
      (> x (dec (count (get disk 0)))) false
      (> y (dec (count disk))) false
      :else (= (.charAt ^String (get disk y) x) \1))))

(defn seen? [c seen]
  (loop [to-see seen]
    (if (empty? to-see)
        false
        (if (= (first to-see) c)
            true
            (recur (drop 1 to-see))))))

(defn get-occupied-neighbours [c disk seen]
  (reduce
    (fn [ns i]
        (let [cr (with-dir c i)]
          (if (and
                (is-occupied? cr disk)
                (not (seen? cr seen)))
              (conj ns cr)
              ns)))
    [] dirs))

(defn merge-ns [ns1 ns2]
  (->> ns1
       (concat ns2)
       (distinct)
       (vec)))

(defn get-component [c disk seen]
  (if (is-occupied? c disk)
      (let [ns (get-occupied-neighbours c disk seen)]
        (if (empty? ns)
            [c]
            (loop [to-see ns
                   visited [c]]
              (if (empty? to-see)
                visited
                (recur
                  (merge-ns
                    (drop 1 to-see)
                    (get-occupied-neighbours (first to-see) disk visited))
                  (merge-ns visited [(first to-see)]))))))
      []))

(defn clear-pos [c disk]
  (let [row (get disk (second c))
        before (subs row 0 (first c))
        after (subs row (inc (first c)))
        new-row (str before "0" after)]
    (assoc disk (second c) new-row)))


(defn clear-disk-pos [disk pos-coll]
  (loop [to-clear pos-coll
         new-disk disk]
    (if (empty? to-clear)
        new-disk
        (recur (drop 1 to-clear) (clear-pos (first to-clear) new-disk)))))

(defn disk-row-size [disk]
  (count (get disk 1)))

(defn get-coordinate [tick disk]
  (let [size (disk-row-size disk)]
    [(mod tick size) (quot tick size)]))

(defn finished? [tick disk]
  (= tick (* (count disk) (disk-row-size disk))))

(defn get-component-count [disk]
  (loop [tick 0
         d disk
         c 0]
    (if (finished? tick disk)
        c
        (let [coordinate (get-coordinate tick disk)
              comps (get-component coordinate d [])]
          (if (empty? comps)
              (recur (inc tick) d c)
              (recur (inc tick) (clear-disk-pos d comps) (inc c)))))))

(get-component-count (parse-disk "flqrgnkx"))
;=> 1242
(get-component-count (parse-disk "jxqlasbh"))
;=> 1182