(ns advent-of-code.day22
  (gen-class))

;Diagnostics indicate that the local grid computing cluster has been contaminated with the Sporifica Virus. The grid computing cluster is a seemingly-infinite two-dimensional grid of compute nodes. Each node is either clean or infected by the virus.
;To prevent overloading the nodes (which would render them useless to the virus) or detection by system administrators, exactly one virus carrier moves through the network, infecting or cleaning nodes as it moves. The virus carrier is always located on a single node in the network (the current node) and keeps track of the direction it is facing.
;To avoid detection, the virus carrier works in bursts; in each burst, it wakes up, does some work, and goes back to sleep. The following steps are all executed in order one time each burst:
;If the current node is infected, it turns to its right. Otherwise, it turns to its left. (Turning is done in-place); the current node does not change.)
;If the current node is clean, it becomes infected. Otherwise, it becomes cleaned. (This is done after the node is considered for the purposes of changing direction.)
;The virus carrier moves forward one node in the direction it is facing.
;Diagnostics have also provided a map of the node infection status (your puzzle input). Clean nodes are shown as .; infected nodes are shown as #. This map only shows the center of the grid; there are many more nodes beyond those shown, but none of them are currently infected.
;
;The virus carrier begins in the middle of the map facing up.
;
;For example, suppose you are given a map like this:
;
; ..#
; #..
; ...
;Then, the middle of the infinite grid looks like this, with the virus carrier's position marked with [ ]:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . # . . .
; . . . #[.]. . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;The virus carrier is on a clean node, so it turns left, infects the node, and moves left:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . # . . .
; . . .[#]# . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;The virus carrier is on an infected node, so it turns right, cleans the node, and moves up:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . .[.]. # . . .
; . . . . # . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;Four times in a row, the virus carrier finds a clean, infects it, turns left, and moves forward, ending in the same place and still facing up:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . #[#]. # . . .
; . . # # # . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;Now on the same node as before, it sees an infection, which causes it to turn right, clean the node, and move forward:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . # .[.]# . . .
; . . # # # . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;After the above actions, a total of 7 bursts of activity had taken place. Of them, 5 bursts of activity caused an infection.
;
;After a total of 70, the grid looks like this, with the virus carrier facing up:
;
; . . . . . # # . .
; . . . . # . . # .
; . . . # . . . . #
; . . # . #[.]. . #
; . . # . # . . # .
; . . . . . # # . .
; . . . . . . . . .
; . . . . . . . . .
;By this time, 41 bursts of activity caused an infection (though most of those nodes have since been cleaned).
;After a total of 10000 bursts of activity, 5587 bursts will have caused an infection.
;Given your actual map, after 10000 bursts of activity, how many bursts cause a node to become infected? (Do not count nodes that begin infected.)

(use 'advent-of-code.utils)
(def test-input
  "..#
   #..
   ...")

(def test-virus [[1 1] :up])

(defn parse-grid [input]
  (->> input
       (to-lines)
       (map-indexed (fn [y item] (map-indexed (fn [x c] [[x y] c]) item)))
       (reduce concat)
       (filter #(= \# (second %)))))

(defn is-infected? [pos grid]
  (->> grid
       (filter #(= (first %) pos))
       (filter #(= (second %) \#))
       (first)
       (some?)))

(defn turn-right [direction]
  (cond
    (= direction :up) :right
    (= direction :right) :down
    (= direction :down) :left
    (= direction :left) :up))

(defn turn-left [direction]
  (cond
    (= direction :up) :left
    (= direction :left) :down
    (= direction :down) :right
    (= direction :right) :up))

(defn clean-node [pos grid]
  (filter #(not= (first %) pos) grid))
  
(defn infect-node [pos grid]
  (conj grid [pos \#]))

(defn move-virus [pos direction]
  (cond
    (= direction :up) (mapv + pos [0 -1])
    (= direction :down) (mapv + pos [0 1])
    (= direction :right) (mapv + pos [1 0])
    (= direction :left) (mapv + pos [-1 0])))

(defn step [virus grid]
  (let [infected? (is-infected? (first virus) grid)
        new-dir (if infected? (turn-right (second virus)) (turn-left (second virus)))
        grid (if infected? (clean-node (first virus) grid) (infect-node (first virus) grid))]
       [[(move-virus (first virus) new-dir) new-dir] grid (not infected?)]))

(defn compute-bursts [step-count virus grid]
  (loop [c 0
         bc 0
         v virus
         g grid]
    (if (= c step-count)
        bc
        (let [[nv ng b?] (step v g)]
             (recur (inc c) (if b? (inc bc) bc) nv ng)))))

(compute-bursts 10000 test-virus (parse-grid test-input))
; => 5587

(def input
  "#.....##.####.#.#########
   .###..#..#..####.##....#.
   ..#########...###...####.
   .##.#.##..#.#..#.#....###
   ...##....###..#.#..#.###.
   ###..#...######.####.#.#.
   #..###..###..###.###.##..
   .#.#.###.#.#...####..#...
   ##........##.####..##...#
   .#.##..#.#....##.##.##..#
   ###......#..##.####.###.#
   ....#..###..#######.#...#
   #####.....#.##.#..#..####
   .#.###.#.###..##.#..####.
   ..#..##.###...#######....
   .#.##.#.#.#.#...###.#.#..
   ##.###.#.#.###.#......#..
   ###..##.#...#....#..####.
   .#.#.....#..#....##..#..#
   #####.#.##..#...##..#....
   ##..#.#.#.####.#.##...##.
   ..#..#.#.####...#........
   ###.###.##.#..#.##.....#.
   .##..##.##...#..#..#.#..#
   #...####.#.##...#..#.#.##")

(def virus [[12 12] :up])

(compute-bursts 10000 virus (parse-grid input))
; => 5450

;--- Part Two ---
;As you go to remove the virus from the infected nodes, it evolves to resist your attempt.
;
;Now, before it infects a clean node, it will weaken it to disable your defenses. If it encounters an infected node, it will instead flag the node to be cleaned in the future. So:
;
;Clean nodes become weakened.
;Weakened nodes become infected.
;Infected nodes become flagged.
;Flagged nodes become clean.
;Every node is always in exactly one of the above states.
;
;The virus carrier still functions in a similar way, but now uses the following logic during its bursts of action:
;
;Decide which way to turn based on the current node:
;If it is clean, it turns left.
;If it is weakened, it does not turn, and will continue moving in the same direction.
;If it is infected, it turns right.
;If it is flagged, it reverses direction, and will go back the way it came.
;Modify the state of the current node, as described above.
;The virus carrier moves forward one node in the direction it is facing.
;Start with the same map (still using . for clean and # for infected) and still with the virus carrier starting in the middle and facing up.
;
;Using the same initial state as the previous example, and drawing weakened as W and flagged as F, the middle of the infinite grid looks like this, with the virus carrier's position again marked with [ ]:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . # . . .
; . . . #[.]. . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;This is the same as before, since no initial nodes are weakened or flagged. The virus carrier is on a clean node, so it still turns left, instead weakens the node, and moves left:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . # . . .
; . . .[#]W . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;The virus carrier is on an infected node, so it still turns right, instead flags the node, and moves up:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . .[.]. # . . .
; . . . F W . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;This process repeats three more times, ending on the previously-flagged node and facing right:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . W W . # . . .
; . . W[F]W . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;Finding a flagged node, it reverses direction and cleans the node:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . W W . # . . .
; . .[W]. W . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;The weakened node becomes infected, and it continues in the same direction:
;
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . W W . # . . .
; .[.]# . W . . . .
; . . . . . . . . .
; . . . . . . . . .
; . . . . . . . . .
;Of the first 100 bursts, 26 will result in infection. Unfortunately, another feature of this evolved virus is speed; of the first 10000000 bursts, 2511944 will result in infection.
;Given your actual map, after 10000000 bursts of activity, how many bursts cause a node to become infected? (Do not count nodes that begin infected.)

(defn set-node [pos state grid]
  (->> grid
       (filter #(not= pos (first %)))
       (cons [pos state])))

(defn get-node [pos grid]
  (let [node (->> grid
                  (filter #(= (first %) pos))
                  (first))]
    (if (some? node)
        (second node)
        \.)))

(defn step-grid [virus grid]
  (let [position (first virus)
        node (get-node position grid)]
    (cond
      (= node \.) [(set-node position \W grid) false]
      (= node \W) [(set-node position \# grid) true]
      (= node \#) [(set-node position \F grid) false]
      (= node \F) [(set-node position \. grid) false])))

(defn reverse-direction [d]
  (cond
    (= d :left) :right
    (= d :right) :left
    (= d :up) :down
    (= d :down) :up))

(defn step-virus [virus grid]
  (let [position (first virus)
        node (get-node position grid)
        direction (cond
                    (= node \.) (turn-left (second virus))
                    (= node \W) (second virus)
                    (= node \#) (turn-right (second virus))
                    (= node \F) (reverse-direction (second virus)))]
    [(move-virus position direction) direction]))

(defn step [virus grid]
  (let [nv (step-virus virus grid)
        [ng infected?] (step-grid virus grid)]
       [nv ng infected?]))

(defn compute-bursts-to-infect [step-count virus grid]
  (loop [c 0
         ic 0
         v virus
         g grid]
    (if (= c step-count)
        ic
        (let [[nv ng i?] (step v g)
              _ (if (= 0 (mod c 10000)) (println c))]
             (recur (inc c) (if i? (inc ic) ic) nv ng)))))

(compute-bursts-to-infect 100 test-virus (parse-grid test-input))
;=> 26