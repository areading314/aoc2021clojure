(ns aoc2021.core
  (:require [clj-http.client :as client]
            [clojure.string :as str]))

(defn sum [coll] (reduce + coll))

(defn get-input-cache-filename [year puzzle-number]
  (str/join "/" [(System/getenv "AOC_INPUT_CACHE_DIR")
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
                   (str/split-lines)
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
  (let [[dir-string distance-string] (str/split movement-string #" ")
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
                    (str/split-lines)
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
  (vec (map (partial = "1") (str/split str #""))))

(defn arr-to-int [arr]
  (->> arr (map #(if % "1" "0")) (str/join "") (bin-to-int)))

(defn calc-epsgamma [arrs]
  (let [gamma (arr-to-int (most-common arrs true >))
        epsilon (arr-to-int (most-common arrs false <))
        oxygen-rating (arr-to-int (find-best-prefix-match arrs 0 true >))
        co2-scrubber-rating (arr-to-int (find-best-prefix-match arrs 0 false <))]
    (println (* gamma epsilon))
    (println (* oxygen-rating co2-scrubber-rating))))

(defn problem3 [input]
  (->> input (str/split-lines)
       (map str-to-boolarray) (vec) (calc-epsgamma)))

(defn read-board [boardinput]
  (vec (map #(vec (map parse-int (str/split (str/trim %) #"\s+"))) boardinput)))

(defn transpose [m] (apply mapv vector m))

(defn read-problem4-input
  "Reads a collection of lines and returns a map of bingo numbers and boards"
  [input]
  (let [moves (map parse-int (str/split (get input 0) #","))
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
  (let [lines (vec (str/split-lines input))
        {boards :boards moves :moves} (read-problem4-input lines)]
    (println (play-bingo 1 boards moves))
    (println (lose-bingo 1 boards moves nil))))

(defn parse-line [line-input]
  (let [[p1-input p2-input] (str/split line-input #" -> ")
        [x1 y1] (mapv parse-int (str/split p1-input #","))
        [x2 y2] (mapv parse-int (str/split p2-input #","))]
    [x1 y1 x2 y2]))

(defn between [x a b]
  (if (b < a)
    (between x b a)
    (and (>= x a) (<= x b))))

(defn abs [n] (max n (- n)))
(defn dist [a b] (abs (- a b)))

(defn diag-dir [x1 y1 x2 y2]
  (> (* (- y2 y1) (- x2 x1)) 0))

(defn abs-range [a b]
  (range (min a b) (inc (max a b))))

(defn gen-line-coords [x1 y1 x2 y2 include-diag?]
  (cond
    (= x1 x2) (mapv #(vector x1 %) (abs-range y1 y2))
    (= y1 y2) (mapv #(vector % y1) (abs-range x1 x2))
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

(defn show-count [c]
  (if (= c 0) "." (str c)))

(defn print-counts [count-grid]
  (doall (map #(println (str/join (map show-count %))) count-grid)))

(defn problem5 [input]
  (let [line-input (str/split-lines input)
        segments (mapv parse-line line-input)
        problem-size (inc (reduce max (flatten segments)))
        ocean-grid (init-grid problem-size problem-size 0)
        line-map (reduce draw-line-no-diag ocean-grid segments)
        line-map-diag (reduce draw-line-diag ocean-grid segments)
        danger-count (count-danger-coords line-map)
        danger-count-diag (count-danger-coords line-map-diag)]
    (println danger-count)
    (println danger-count-diag)))

(defn lrotate [v n]
  (vec (concat (subvec v n (count v)) (subvec v 0 n))))

(defn sim-lanternfish [lanternfish-map]
  (let [new-fish (get lanternfish-map 0)]
    (update (lrotate lanternfish-map 1) 6 + new-fish)))

(defn parse-lanternfish [input]
  (let [fish-timers (map parse-int (str/split input #","))]
    (mapv #(or (get (frequencies fish-timers) %) 0) (range 9))))

(defn get-lanternfish-count [l n]
  (sum (nth (iterate sim-lanternfish l) n)))

(defn problem6 [input]
  (let [lanternfish (parse-lanternfish (str/trim input))]
    (doall (map #(println (get-lanternfish-count lanternfish %)) [80 256]))))

(defn calc-fuel [ints c]
  (sum (map #(dist % c) ints)))

(defn triang-num [n]
  (/ (* (inc n) n) 2))

(defn calc-fuel-crab [ints c]
  (sum (map #(triang-num (dist % c)) ints)))

(defn find-min-fuel [ints fuel-func]
  (let [maxint (reduce max ints)]
    (reduce min (map #(fuel-func ints %) (range maxint)))))

(defn problem7 [input]
  (let [ints (as-> input s
                   (str/trim s)
                   (str/split s #",")
                   (mapv parse-int s))]
    (println (find-min-fuel ints calc-fuel))
    (println (find-min-fuel ints calc-fuel-crab))))

(defn get-freqs [strs]
  (frequencies (flatten (map #(str/split % #"") strs))))

(def freqstr-to-int
  {"467889"  0
   "89"      1
   "47788"   2
   "77889"   3
   "6789"    4
   "67789"   5
   "467789"  6
   "889"     7
   "4677889" 8
   "677889"  9})

(defn str-to-freqstr [str freqs]
  (str/join "" (sort (map freqs (str/split str #"")))))

(defn normalize-str [str]
  (str/join "" (sort (str/split str #""))))

(defn mapping-from-strs [strs]
  (let [sstrs (map normalize-str strs)
        freqs (get-freqs sstrs)
        freqstrs (map #(str-to-freqstr % freqs) sstrs)
        ints (map freqstr-to-int freqstrs)]
    (zipmap sstrs ints)))

(defn decode-wires [observed code]
  (let [translate-map (mapping-from-strs (map normalize-str observed))]
    (str/join "" (map (comp translate-map normalize-str) code))))

(defn split-ws [str]
  (str/split str #"\s+"))

(defn problem8 [input]
  (let [unique-segments [2 3 4 7]
        lines (str/split-lines (str/trim input))
        grps (map #(str/split % #" \| ") lines)
        info (map first grps)
        nums (map second grps)
        codes (map split-ws nums)
        desired (filter #(some #{(count %)} unique-segments) (flatten codes))
        infos (map split-ws info)
        code-output (map (comp parse-int decode-wires) infos codes)]
    (println (count desired))
    (println (sum code-output))))


(defn read-heightmap-row [s]
  (mapv parse-int (str/split s #"")))

(defn read-heightmap [input]
  (mapv read-heightmap-row (str/split-lines (str/trim input))))

(defn above [p]
  (let [[x y] p]
    [x (dec y)]))

(defn below [p]
  (let [[x y] p]
    [x (inc y)]))

(defn left [p]
  (let [[x y] p]
    [(dec x) y]))

(defn right [p]
  (let [[x y] p]
    [(inc x) y]))

(defn get-xy [m p]
  (get-in m (reverse p)))

(defn get-adjacent [m p]
  (remove #(nil? (get-xy m %)) [(above p) (below p) (left p) (right p)]))

(defn higher-adjacents [m p s]
  (remove #(or (= 9 (get-xy m %)) (scontains? s %)) (get-adjacent m p)))

(defn measure-basin [m p]
  (loop [ps [p]
         seen (set [])]
    (let [current (first ps)
          successors (higher-adjacents m current seen)
          next-ps (concat (next ps) successors)
          next-seen (conj seen current)]
      (if (empty? next-ps)
        (count next-seen)
        (recur next-ps next-seen)))))

(defn is-low-point? [m p]
  (let [adjacents (get-adjacent m p)
        adjacent-heights (map #(get-xy m %) adjacents)
        non-nil-adjacents (remove nil? adjacent-heights)]
    (every? #(< (get-xy m p) %) non-nil-adjacents)))

(defn gen-points [width height]
  (for [x (range width)
        y (range height)] [x y]))

(defn problem9 [input]
  (let [height-map (read-heightmap input)
        height (count height-map)
        width (count (first height-map))
        low-points (filter #(is-low-point? height-map %) (gen-points width height))
        low-heights (map #(get-xy height-map %) low-points)]
    (println (sum (map inc low-heights)))
    (println (->> low-points (map #(measure-basin height-map %)) sort reverse (take 3) (reduce *))))
  )

(defn check-match [c stack]
  (if (= c (first stack))
    (next stack) c))

(defn diagnose-char [stack c]
  (if (string? stack)
    stack
    (case c
      "(" (cons ")" stack) "[" (cons "]" stack)
      "<" (cons ">" stack) "{" (cons "}" stack)
      (check-match c stack)
      ))
  )

(defn diagnose-line [line-seq]
  (reduce diagnose-char (list) line-seq))

(def char-score {")" 3 "]" 57 "}" 1197 ">" 25137})
(defn middle [coll] (nth coll (/ (count coll) 2)))
(def stack-scores {")" 1 "]" 2 "}" 3 ">" 4})
(defn ssreduce [n e] (+ e (* 5 n)))
(defn stack-score [stack]
  (reduce ssreduce 0 (map stack-scores stack)))

(defn problem10 [input]
  (let [lines (str/split-lines (str/trim input))
        seqs (map #(str/split % #"") lines)
        diagnoses (map diagnose-line seqs)]
    (println (sum (map char-score (filter string? diagnoses))))
    (println (->> diagnoses (remove string?) (map stack-score) sort middle))))

(let [functions [problem1 problem2 problem3 problem4
                 problem5 problem6 problem7 problem8
                 problem9 problem10]]
  (defn -main
    []
    (print "Enter problem to solve: ")
    (flush)
    (let [problemNumber (Integer/parseInt (read-line))]
      ((get functions (- problemNumber 1))
       (get-puzzle-input 2021 problemNumber)))))