(ns advent-of-code.day13
  (gen-class))

;--- Day 13: Packet Scanners ---
;
;You need to cross a vast firewall. The firewall consists of several layers, each with a security scanner that moves back and forth across the layer. To succeed, you must not be detected by a scanner.
;
;By studying the firewall briefly, you are able to record (in your puzzle input) the depth of each layer and the range of the scanning area for the scanner within it, written as depth: range. Each layer has a thickness of exactly 1. A layer at depth 0 begins immediately inside the firewall; a layer at depth 1 would start immediately after that.
;
;For example, suppose you've recorded the following:
;
;0: 3
;1: 2
;4: 4
;6: 4
(use 'advent-of-code.utils)

(def input
  "0: 3
   1: 2
   4: 4
   6: 4")

(defn parse-input [s]
  (->> s
       (to-lines)
       (map #(re-seq #"\d+" %))
       (map #(list (to-digit (first %)) (to-digit (second %))))))

(defn max-layer-num [l]
  (->> l
       (map #(first %))
       (apply max)))

(defn fill-in [l v]
  (reduce
    (fn [layers item]
      (assoc layers (first item) (second item)))
    v l))

(defn to-firewall-vec [l]
  (->>
    (repeat 0)
    (take (max-layer-num l))
    (vec)
    (fill-in l)))

;This means that there is a layer immediately inside the firewall (with range 3), a second layer immediately after that (with range 2), a third layer which begins at depth 4 (with range 4), and a fourth layer which begins at depth 6 (also with range 4). Visually, it might look like this:
;
;0   1   2   3   4   5   6
;[ ] [ ] ... ... [ ] ... []
;[ ] [ ]         [ ]     []
;[ ]             [ ]     []
;[ ]     []
;Within each layer, a security scanner moves back and forth within its range. Each security scanner starts at the top and moves down until it reaches the bottom, then moves up until it reaches the top, and repeats. A security scanner takes one picosecond to move one step. Drawing scanners as S, the first few picoseconds look like this:
;
;
;Picosecond 0:
;0   1   2   3   4   5   6
;[S] [S] ... ... [S] ... [S]
;[ ] [ ]         [ ]     []
;[ ]             [ ]     []
;[ ]     []
;
;Picosecond 1:
;0   1   2   3   4   5   6
;[ ] [ ] ... ... [ ] ... []
;[S] [S]         [S]     [S]
;[ ]             [ ]     []
;[ ]     []
;
;Picosecond 2:
;0   1   2   3   4   5   6
;[ ] [S] ... ... [ ] ... []
;[ ] [ ]         [ ]     []
;[S]             [S]     [S]
;[ ]     []
;
;Picosecond 3:
;0   1   2   3   4   5   6
;[ ] [ ] ... ... [ ] ... []
;[S] [S]         [ ]     []
;[ ]             [ ]     []
;[S]     [S]

;. 1 2 3 4 5 6 7 8 9 . 1 2 3 4 5 6 7 8 9 . tick
;-----------------------------------------
;0 1|0 1|0 1|0 1|0 1|0 1|0 1|0 1|0 1|0 1|0 range 2
;-----------------------------------------
;0 1 2 1|0 1 2 1|0 1 2 1|0 1 2 1|0 1 2 1|0 range 3
;-----------------------------------------
;0 1 2 3 2 1|0 1 2 3 2 1|0 1 2 3 2 1|0 1 2 range 4
;-----------------------------------------
;0 1 2 3 4 3 2 1|0 1 2 3 4 3 2 1|0 1 2 3 4 range 5
;
; 2*(n - 1) - size
; tick % size -> rest
; half -> range - 1
; rest < range -> up   = rest
; rest > range -> down = half - (rest % (range - 1))

(defn calculate-scanner-pos [tick range]
  (if (<= range 1)
      1
      (let [half (- range 1)
            size (* 2 half)
            rest (mod tick size)]
        (if (< rest half)
            rest
            (- half (mod rest (- range 1)))))))

(defn is-caught? [tick range]
  (= (calculate-scanner-pos tick range) 0))

;Your plan is to hitch a ride on a packet about to move through the firewall. The packet will travel along the top of each layer, and it moves at one layer per picosecond. Each picosecond, the packet moves one layer forward (its first move takes it into layer 0), and then the scanners move one step. If there is a scanner at the top of the layer as your packet enters it, you are caught. (If a scanner moves into the top of its layer while you are there, you are not caught: it doesn't have time to notice you before you leave.) If you were to do this in the configuration above, marking your current position with parentheses, your passage through the firewall would look like this:
;
;Initial state:
;0   1   2   3   4   5   6
;[S] [S] ... ... [S] ... [S]
;[ ] [ ]         [ ]     []
;[ ]             [ ]     []
;[ ]     []
;
;Picosecond 0:
;0   1   2   3   4   5   6
;(S) [S] ... ... [S] ... [S]
;[ ] [ ]         [ ]     []
;[ ]             [ ]     []
;[ ]     []
;
;0   1   2   3   4   5   6
;( ) [ ] ... ... [ ] ... []
;[S] [S]         [S]     [S]
;[ ]             [ ]     []
;[ ]     []
;
;
;Picosecond 1:
;0   1   2   3   4   5   6
;[ ] ( ) ... ... [ ] ... []
;[S] [S]         [S]     [S]
;[ ]             [ ]     []
;[ ]     []
;
;0   1   2   3   4   5   6
;[ ] (S) ... ... [ ] ... []
;[ ] [ ]         [ ]     []
;[S]             [S]     [S]
;[ ]     []
;
;
;Picosecond 2:
;0   1   2   3   4   5   6
;[ ] [S] (.) ... [ ] ... []
;[ ] [ ]         [ ]     []
;[S]             [S]     [S]
;[ ]     []
;
;0   1   2   3   4   5   6
;[ ] [ ] (.) ... [ ] ... []
;[S] [S]         [ ]     []
;[ ]             [ ]     []
;[S]     [S]
;
;
;Picosecond 3:
;0   1   2   3   4   5   6
;[ ] [ ] ... (.) [ ] ... []
;[S] [S]         [ ]     []
;[ ]             [ ]     []
;[S]     [S]
;
;0   1   2   3   4   5   6
;[S] [S] ... (.) [ ] ... []
;[ ] [ ]         [ ]     []
;[ ]             [S]     [S]
;[ ]     []
;
;
;Picosecond 4:
;0   1   2   3   4   5   6
;[S] [S] ... ... ( ) ... []
;[ ] [ ]         [ ]     []
;[ ]             [S]     [S]
;[ ]     []
;
;0   1   2   3   4   5   6
;[ ] [ ] ... ... ( ) ... []
;[S] [S]         [S]     [S]
;[ ]             [ ]     []
;[ ]     []
;
;
;Picosecond 5:
;0   1   2   3   4   5   6
;[ ] [ ] ... ... [ ] (.) []
;[S] [S]         [S]     [S]
;[ ]             [ ]     []
;[ ]     []
;
;0   1   2   3   4   5   6
;[ ] [S] ... ... [S] (.) [S]
;[ ] [ ]         [ ]     []
;[S]             [ ]     []
;[ ]     []
;
;
;Picosecond 6:
;0   1   2   3   4   5   6
;[ ] [S] ... ... [S] ... (S)
;[ ] [ ]         [ ]     []
;[S]             [ ]     []
;[ ]     []
;
;0   1   2   3   4   5   6
;[ ] [ ] ... ... [ ] ... ()
;[S] [S]         [S]     [S]
;[ ]             [ ]     []
;[ ]     []

(defn calculate-severity [fw-vec]
  (loop [tick 0
         severity 0]
    (if (= tick (count fw-vec))
        severity
        (let [range (get fw-vec tick)]
             (if (is-caught? tick range)
                 (recur (inc tick) (+ severity (* tick range)))
                 (recur (inc tick) severity))))))

(defn process [input]
  (->> input
       (parse-input)
       (to-firewall-vec)
       (calculate-severity)))

(process input)
; => 24
(process (slurp "src/advent_of_code/day13.in"))
; => 1960

;--- Part Two ---
;
;Now, you need to pass through the firewall without being caught - easier said than done.
;You can't control the speed of the packet, but you can delay it any number of picoseconds. For each picosecond you delay the packet before beginning your trip, all security scanners move one step. You're not in the firewall during this time; you don't enter layer 0 until you stop delaying the packet.
;In the example above, if you delay 10 picoseconds (picoseconds 0 - 9), you won't get caught:
;
;State after delaying:
;0   1   2   3   4   5   6
;[ ] [S] ... ... [ ] ... []
;[ ] [ ]         [ ]     []
;[S]             [S]     [S]
;[ ]     []
;
;Picosecond 10:
;0   1   2   3   4   5   6
;( ) [S] ... ... [ ] ... []
;[ ] [ ]         [ ]     []
;[S]             [S]     [S]
;[ ]     []
;
;0   1   2   3   4   5   6
;( ) [ ] ... ... [ ] ... []
;[S] [S]         [S]     [S]
;[ ]             [ ]     []
;[ ]     []
;
;
;Picosecond 11:
;0   1   2   3   4   5   6
;[ ] ( ) ... ... [ ] ... []
;[S] [S]         [S]     [S]
;[ ]             [ ]     []
;[ ]     []
;
;0   1   2   3   4   5   6
;[S] (S) ... ... [S] ... [S]
;[ ] [ ]         [ ]     []
;[ ]             [ ]     []
;[ ]     []
;
;
;Picosecond 12:
;0   1   2   3   4   5   6
;[S] [S] (.) ... [S] ... [S]
;[ ] [ ]         [ ]     []
;[ ]             [ ]     []
;[ ]     []
;
;0   1   2   3   4   5   6
;[ ] [ ] (.) ... [ ] ... []
;[S] [S]         [S]     [S]
;[ ]             [ ]     []
;[ ]     []
;
;
;Picosecond 13:
;0   1   2   3   4   5   6
;[ ] [ ] ... (.) [ ] ... []
;[S] [S]         [S]     [S]
;[ ]             [ ]     []
;[ ]     []
;
;0   1   2   3   4   5   6
;[ ] [S] ... (.) [ ] ... []
;[ ] [ ]         [ ]     []
;[S]             [S]     [S]
;[ ]     []
;
;
;Picosecond 14:
;0   1   2   3   4   5   6
;[ ] [S] ... ... ( ) ... []
;[ ] [ ]         [ ]     []
;[S]             [S]     [S]
;[ ]     []
;
;0   1   2   3   4   5   6
;[ ] [ ] ... ... ( ) ... []
;[S] [S]         [ ]     []
;[ ]             [ ]     []
;[S]     [S]
;
;
;Picosecond 15:
;0   1   2   3   4   5   6
;[ ] [ ] ... ... [ ] (.) []
;[S] [S]         [ ]     []
;[ ]             [ ]     []
;[S]     [S]
;
;0   1   2   3   4   5   6
;[S] [S] ... ... [ ] (.) []
;[ ] [ ]         [ ]     []
;[ ]             [S]     [S]
;[ ]     []
;
;
;Picosecond 16:
;0   1   2   3   4   5   6
;[S] [S] ... ... [ ] ... ()
;[ ] [ ]         [ ]     []
;[ ]             [S]     [S]
;[ ]     []
;
;0   1   2   3   4   5   6
;[ ] [ ] ... ... [ ] ... ()
;[S] [S]         [S]     [S]
;[ ]             [ ]     []
;[ ]     []
;Because all smaller delays would get you caught, the fewest number of picoseconds you would need to delay to get through safely is 10.
;What is the fewest number of picoseconds that you need to delay the packet to pass through the firewall without being caught?

(defn is-safe? [delay fw-vec]
  (loop [tick 0]
    (if (= tick (count fw-vec))
        true
        (if (is-caught? (+ tick delay) (get fw-vec tick))
            false
            (recur (inc tick))))))

(defn calculate-safe-delay [fw-vec]
  (loop [delay 0]
    (if (is-safe? delay fw-vec)
      delay
      (recur (inc delay)))))

(defn process2 [input]
  (->> input
       (parse-input)
       (to-firewall-vec)
       (calculate-safe-delay)))

(process2 input)
; => 10
(process2 (slurp "src/advent_of_code/day13.in"))
; => 3903378