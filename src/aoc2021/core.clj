(ns aoc2021.core
  (:require [clj-http.client :as client]))

(defn sum [coll] (reduce + coll))

(defn get-input-cache-filename [year puzzle-number]
  (clojure.string/join "/" [(System/getenv "AOC_INPUT_CACHE_DIR")
                            (format "input%s.problem%s" year puzzle-number)]))

(defn get-puzzle-input [year puzzle-number]
  (let [cached-file (get-input-cache-filename year puzzle-number)]
    (if (.exists (clojure.java.io/file cached-file))
      (slurp cached-file)
      (let [session-cookie (format "session=%s" (slurp (System/getenv "AOC_COOKIE_FILE")))
            download-input (get (client/get
                                  (format "https://adventofcode.com/%s/day/%s/input" year puzzle-number)
                                  {:headers {:cookie session-cookie}}) :body)]
        (spit cached-file download-input) download-input))))

(defn count-increases [coll]
  (count (filter (partial apply <)
                 (partition 2 1 coll))))

(defn moving-sum [n coll]
  (map sum (partition n 1 coll)))

(defn parse-int [s] (Long/parseLong s))

(defn problem1 [input]
  (let [integers (->>
                   input
                   (clojure.string/split-lines)
                   (map parse-int))]
    (println (count-increases integers))
    (println (count-increases (moving-sum 3 integers)))))

(defstruct ship :h :x :aim)
(defstruct position :h :x)

(defn add-movements1 [p1 p2]
  (struct position
          (+ (get p1 :h) (get p2 :h))
          (+ (get p1 :x) (get p2 :x))))

(defn parse-movement [movement-string]
  (let [[dir-string distance-string] (clojure.string/split movement-string #" ")
        distance (parse-int distance-string)]
    (case dir-string
      "forward" (struct position 0 distance)
      "backward" (struct position 0 (- distance))
      "down" (struct position distance 0)
      "up" (struct position (- distance) 0))))

(defn add-movements2 [s p]
  (struct ship
          (+ (get s :h) (* (get p :x) (get s :aim)))
          (+ (get s :x) (get p :x))
          (+ (get s :aim) (get p :h))))

(defn problem2 [input]
  (let [movements (->>
                    input
                    (clojure.string/split-lines)
                    (map parse-movement))
        finalpos1 (reduce add-movements1 movements)
        finalpos2 (reduce add-movements2 (struct ship 0 0 0) movements)]
    (println (* (get finalpos1 :x) (get finalpos1 :h)))
    (println (* (get finalpos2 :x) (get finalpos2 :h)))))


(defn bin-to-int [binstring]
  (Integer/parseInt binstring 2))

(defn combine-bin-array [c a]
  (vec (map (fn [cc aa] (if aa (+ cc 1) cc)) c a)))

(defn most-common [arrs tiebreaker op]
  (let [total-count (count arrs)
        one-count (reduce combine-bin-array (vec (repeat (count (get arrs 0)) 0)) arrs)
        zero-count (vec (map #(- total-count %) one-count))]
    (vec (map (fn [zc oc] (cond (= zc oc) tiebreaker
                                (op zc oc) false
                                (op oc zc) true)) zero-count one-count))))

(defn find-best-prefix-match [arrs idx tiebreaker op]
  (if (<= (count arrs) 1)
    (get arrs 0)
    (let [mc (get (most-common arrs tiebreaker op) idx)
          newarrs (vec (filter #(= (get % idx) mc) arrs))]
      (find-best-prefix-match newarrs (+ idx 1) tiebreaker op))))

(defn str-to-boolarray [str]
  (vec (map (partial = "1") (clojure.string/split str #""))))

(defn arr-to-int [arr]
  (->> arr (map #(if % "1" "0")) (clojure.string/join "") (bin-to-int)))

(defn calc-epsgamma [arrs]
  (let [gamma (arr-to-int (most-common arrs true >))
        epsilon (arr-to-int (most-common arrs false <))
        oxygen-rating (arr-to-int (find-best-prefix-match arrs 0 true >))
        co2-scrubber-rating (arr-to-int (find-best-prefix-match arrs 0 false <))]
    (println (* gamma epsilon))
    (println (* oxygen-rating co2-scrubber-rating))))

(defn problem3 [input]
  (->> input (clojure.string/split-lines)
       (map str-to-boolarray) (vec) (calc-epsgamma)))

(defn read-board [boardinput]
  (vec (map #(vec (map parse-int (clojure.string/split (clojure.string/trim %) #"\s+"))) boardinput)))

(defn transpose [m] (apply mapv vector m))

(defn read-problem4-input
  "Reads a collection of lines and returns a map of bingo numbers and boards"
  [input]
  (let [moves (map parse-int (clojure.string/split (get input 0) #","))
        boards (map read-board (partition 5 6 (drop 2 input)))]
    {:moves (vec moves) :boards (vec boards)}))

(defn any-true? [coll]
  (or (some identity coll) false))

(defn every-true? [coll]
  (every? identity coll))

(defn scontains? [coll k]
  (any-true? (map (partial = k) coll)))

(defn apply-bingo-moves [board moves]
  (mapv (fn [r] (mapv #(scontains? moves %) r)) board))

(defn diag [m]
  (map #(get (get m %) %) (range (count m))))

(defn check-bingo [board moves]
  (let [truthmap (apply-bingo-moves board moves)
        horizonal (any-true? (map every-true? truthmap))
        vertical (any-true? (map every-true? (transpose truthmap)))
        diag1 (every-true? (diag truthmap))
        diag2 (every-true? (diag (transpose truthmap)))]
    (or horizonal vertical diag1 diag2)))

(defn get-unmarked-squares [board moves]
  (filter #(not (scontains? (vec moves) %)) (flatten board)))


(defn score-bingo [board moves]
  (let [current-move (last moves)]
    (if (check-bingo board moves)
      (* current-move (sum (get-unmarked-squares board moves))) 0)))

(defn is-winning? [board moves]
  (> (score-bingo board moves) 0))

(def is-not-winning? (complement is-winning?))

(defn winning-scores [boards moves]
  (->> boards
       (map #(score-bingo % moves))
       (filter #(< 0 %))))

(defn losing-games [boards moves]
  (filter #(is-not-winning? % moves) boards))

(defn play-bingo [move boards moves]
  (let [current-moves (take move moves)]
    (or (first (winning-scores boards current-moves))
        (play-bingo (inc move) boards moves))))

(defn lose-bingo [move boards moves last-win-score]
  (let [current-moves (take move moves)
        win-score (or (first (winning-scores boards current-moves))
                      last-win-score)]
    (if (> move (count moves))
      win-score
      (lose-bingo (inc move) (losing-games boards current-moves) moves win-score))))

(defn problem4 [input]
  (let [lines (vec (clojure.string/split-lines input))
        {boards :boards moves :moves} (read-problem4-input lines)]
    (println (play-bingo 1 boards moves))
    (println (lose-bingo 1 boards moves nil))))

(defn parse-line [line-input]
  (let [[p1-input p2-input] (clojure.string/split line-input #" -> ")
        [x1 y1] (mapv parse-int (clojure.string/split p1-input #","))
        [x2 y2] (mapv parse-int (clojure.string/split p2-input #","))]
    [x1 y1 x2 y2]))

(defn between [x lower upper]
  (if (upper < lower)
    (between x upper lower)
    (and (>= x lower) (<= x upper))))

(defn abs [n] (max n (- n)))

(defn diag-dir [x1 y1 x2 y2]
  (> (* (- y2 y1) (- x2 x1)) 0))

(defn gen-line-coords [x1 y1 x2 y2 include-diag?]
    (cond
     (= x1 x2) (mapv #(vector x1 %) (range (min y1 y2) (inc (max y1 y2))))
     (= y1 y2) (mapv #(vector % y1) (range (min x1 x2) (inc (max x1 x2))))
     (not include-diag?) []
     (diag-dir x1 y1 x2 y2) (mapv #(vector (+ (min x1 x2) %1) (+ (min y1 y2) %1)) (range (inc (abs (- x2 x1)))))
     :else (mapv #(vector (+ (min x1 x2) %1) (- (max y1 y2) %1)) (range (inc (abs (- x2 x1)))))))

(defn add-coord-to-grid [grid coord]
  (assoc-in grid coord (inc (get-in grid coord))))

(defn draw-line [canvas segment include-diag?]
  (let [[x1 y1 x2 y2] segment
        line-coords (gen-line-coords x1 y1 x2 y2 include-diag?)]
    (reduce add-coord-to-grid canvas line-coords)))

(def draw-line-no-diag #(draw-line %1 %2 false))
(def draw-line-diag #(draw-line %1 %2 true))

(defn count-danger-coords [line-map]
  (count (filter (partial < 1) (flatten line-map))))

(defn init-grid [size-x size-y val]
  (vec (repeat size-y (vec (repeat size-x val)))))

(defn problem5 [input]
  (let [line-input (clojure.string/split-lines input)
        segments (mapv parse-line line-input)
        problem-size (inc (reduce max (flatten segments)))
        ocean-grid (init-grid problem-size problem-size 0)
        line-map (reduce draw-line-no-diag ocean-grid segments)
        line-map-diag (reduce draw-line-diag ocean-grid segments)
        danger-count (count-danger-coords line-map)
        danger-count-diag (count-danger-coords line-map-diag)]
    (println danger-count)
    (println danger-count-diag)))

(let [functions [problem1 problem2 problem3 problem4
                 problem5]]
  (defn -main
    []
    (print "Enter problem to solve: ")
    (flush)
    (let [problemNumber (Integer/parseInt (read-line))]
      ((get functions (- problemNumber 1))
       (get-puzzle-input 2021 problemNumber)))))