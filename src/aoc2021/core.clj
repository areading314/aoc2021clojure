(ns aoc2021.core
  (:require [clj-http.client :as client]
            [clojure.string :as str]
            [shams.priority-queue :as pq]
            [clojure.math.numeric-tower :as math]))

(defn sum [coll] (reduce + coll))

(defn str-to-chars [string] (str/split string #""))

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

(def adjacents [[0 1] [1 0] [-1 0] [0 -1]])
(defn get-xy [m p]
  (get-in m (reverse p)))

(defn add-points [a b]
  (let [[xa ya] a [xb yb] b]
    [(+ xa xb) (+ ya yb)]))

(defn get-adjacent [m p]
  (remove #(nil? (get-xy m %)) (map #(add-points p %) adjacents)))

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

(defn map-grid [f grid]
  (mapv #(mapv f %) grid))

(defn map-coords [f grid]
  (map-indexed #(map-indexed (fn [x y] (f %1 x y)) %2) grid))

(def surr [[0 1] [1 0] [-1 0] [0 -1] [-1 -1] [1 1] [1 -1] [-1 1]])

(defn outside-bounds-pred [grid]
  (fn [p] (nil? (get-in grid p))))

(defn get-surrounding-coords [p grid]
  (let [[x y] p
        pred (outside-bounds-pred grid)]
    [[x (inc y)]]
    (remove pred (map #(add-points p %) surr))))

(defn expand-flashes [coll grid]
  (mapcat #(get-surrounding-coords % grid) coll))

(defn get-flash-coords [grid already-flashed]
  (remove #(scontains? already-flashed %)
          (remove nil? (apply concat (map-coords #(if (> %3 9) [%1 %2] nil) grid)))))

(defn process-flashes [already-flashed grid]
  (let [new-flashes (get-flash-coords grid already-flashed)
        new-grid (reduce #(update-in %1 %2 inc) grid (expand-flashes new-flashes grid))]
    (if (empty? new-flashes)
      (list already-flashed new-grid)
      (process-flashes (into already-flashed new-flashes) new-grid))))

(defn set-flashed [grid val]
  (map-grid #(if (> % 9) val %) grid))

(defn step-octopi [input-list]
  (let [[energy-map flash-count] input-list
        new-energies (map-grid inc energy-map)
        [flashed newgrid] (process-flashes #{} new-energies)
        finalgrid (set-flashed newgrid 0)]
    (list finalgrid (+ (count flashed) flash-count))))

(defn sum-grid [g] (sum (map sum g)))

(defn get-first-index [pred coll]
  (first (keep-indexed (fn [i x] (if (pred x) i)) coll)))

(defn all-flashed-index [r]
  (get-first-index #(= 0 (sum-grid (first %))) r))

(defn problem11 [input]
  (let [grid (read-heightmap (str/trim input))]
    (println (second (nth (iterate step-octopi (list grid 0)) 100)))
    (println (all-flashed-index (iterate step-octopi (list grid 0))))))

(defn is-small-cave? [cave]
  (= (str/lower-case cave) cave))

(defn parse-cave [cave-lines]
  (reduce (fn [m [k v]]
            (update (update m k (fnil conj []) v) v (fnil conj []) k))
          {} (map #(str/split % #"-") cave-lines)))

(defn twice-visitable? [cave]
  (not (= cave "start")))

(defn traverse-cave [start end cave-map small-caves-visited twice-visited-cave]
  (let [adjacent-caves (get cave-map start)
        new-visited-set (if (is-small-cave? start) (conj small-caves-visited start) small-caves-visited)]
    (if (= start end) 1
                      (+ (sum (map #(traverse-cave % end cave-map new-visited-set twice-visited-cave)
                                   (remove (partial scontains? new-visited-set) adjacent-caves)))
                         (if (nil? twice-visited-cave)
                           (sum (map #(traverse-cave % end cave-map new-visited-set %)
                                     (remove #(not (scontains? new-visited-set %)) (filter twice-visitable? adjacent-caves))))
                           0)))))

(defn problem12 [input]
  (let [cave-lines (->> input str/trim str/split-lines)
        cave (parse-cave cave-lines)]
    (println (traverse-cave "start" "end" cave #{} :dummy))
    (println (traverse-cave "start" "end" cave #{} nil))))

(defn is-whitespace? [input]
  (re-matches #"\s*" input))

(def is-not-whitespace? (complement is-whitespace?))

(defn parse-fold-instruction [input]
  (if (str/includes? input "x")
    [:x (parse-int (str/replace input "fold along x=" ""))]
    [:y (parse-int (str/replace input "fold along y=" ""))]))

(defn reflect-point [point fold]
  (let [[x y] point
        [dir fold-dist] fold]
    (if (= dir :x)
      [(min x (- fold-dist (dist x fold-dist))) y]
      [x (min y (- fold-dist (dist y fold-dist)))])))

(defn print-points [points]
  (let [max-x (inc (reduce max (map first points)))
        max-y (inc (reduce max (map second points)))]
    (str/join "\n" (for [y (range max-y)]
                     (str/join (map #(if (scontains? points [% y]) "#" ".") (range max-x)))))))

(defn fold-paper [paper fold]
  (into #{} (map #(reflect-point % fold) paper)))

(defn parse-point [line]
  (vec (map parse-int (str/split line #","))))

(defn read-dots [input-lines]
  (into #{} (map parse-point input-lines)))

(defn read-p13-input [input]
  (let [lines (str/split-lines (str/trim input))
        dots-input (take-while is-not-whitespace? lines)
        folding-input (next (drop-while is-not-whitespace? lines))]
    [(read-dots dots-input)
     (map parse-fold-instruction folding-input)]))

(defn problem13 [input]
  (let [[dots folds] (read-p13-input input)]
    (println (count (fold-paper dots (first folds))))
    (println (print-points (reduce fold-paper dots folds)))))

(defn read-polymer-template [lines]
  (into {} (map #(str/split % #" -> ") lines)))

(defn merge-polymer [orig new]
  (remove nil? (map #(if (even? %) (nth orig (/ % 2) nil)
                                   (nth new (/ % 2) nil)) (range (* 2 (count orig))))))

(defn read-p14-input [input]
  (let [lines (str/split-lines (str/trim input))]
    [(first lines) (read-polymer-template (drop 2 lines))]))

(defn analyze-p14-result [freqs]
  (let [maxel (apply max (vals freqs))
        minel (apply min (vals freqs))]
    (- maxel minel)))

(defn merge-freqs [f1 f2]
  (into f1 (map #(vector % (+ (get f2 %) (get f1 % 0))) (keys f2))))

(def evolve-freqs
  (memoize
    (fn [l i t]
      (let [[lettera letterb] l
            lookup-str (str/join [lettera letterb])
            middle-letter (get t lookup-str)]
        (if (= i 0)
          (if (= lettera letterb)
            {lettera 2}
            {lettera 1 letterb 1})
          (update (merge-freqs (evolve-freqs [lettera middle-letter] (dec i) t)
                               (evolve-freqs [middle-letter letterb] (dec i) t))
                  middle-letter
                  dec))))
    ))

(defn get-iteration-evolved-polymer [n polymer template]
  (let [groups (partition 2 1 (str-to-chars polymer))
        freqs (map #(evolve-freqs % n template) groups)
        middle-letters (map first groups)
        middle-letter-freq-pairs (map vector freqs middle-letters)]
    (reduce #(update (merge-freqs %1 (first %2)) (second %2) dec)
            (first freqs)
            (drop 1 middle-letter-freq-pairs))))


(defn problem14 [input]
  (let [[polymer template] (read-p14-input input)]
    (println (analyze-p14-result (get-iteration-evolved-polymer 10 polymer template)))
    (println (analyze-p14-result (get-iteration-evolved-polymer 40 polymer template)))))


(defn find-end-pos [cave-map]
  [(dec (count (first cave-map))) (dec (count cave-map))])


(defn unvisited-neighbors [point visited grid]
  (remove #(contains? visited %) (get-adjacent grid point)))

(defn show-distances [distances end-pos]
  (let [[width height] end-pos]
    (str/join "\n" (map (fn [y]
                          (str/join "" (map #(format "%5s" (get distances [% y])) (range (inc width)))))
                        (range (inc height))))))

(defn make-heap [distances]
  (into (sorted-set) (map #(vector %1 %2) (vals distances) (keys distances))))

(defn dijkstra-distance [cave-map start-pos end-pos]
  (let [[end-x end-y] (map inc end-pos)
        unvisited-init (into [] (gen-points end-x end-y))
        distance-map-init (assoc (into {} (map #(vector % ##Inf) unvisited-init)) start-pos 0)]
    (loop [unvisited (make-heap distance-map-init)
           visited #{}
           current-node start-pos
           neighbors (into [] (unvisited-neighbors start-pos visited cave-map))
           distances distance-map-init
           n 0]
      (cond
        (or (contains? visited end-pos) (and (= current-node end-pos) (empty? unvisited))) (get distances end-pos)
        (empty? neighbors) (let [[_ new-node] (first unvisited)
                                 new-unvisited (disj unvisited [_ new-node])
                                 new-visited (conj visited current-node)]
                             (recur new-unvisited
                                    (conj visited current-node)
                                    new-node
                                    (unvisited-neighbors new-node new-visited cave-map)
                                    distances
                                    (inc n)))
        :else (let [this-neighbor (first neighbors)
                    current-distance (get distances this-neighbor)
                    this-distance (min current-distance (+ (get distances current-node) (get-xy cave-map this-neighbor)))
                    new-distances (assoc distances this-neighbor this-distance)]
                (recur (conj (disj unvisited [current-distance this-neighbor]) [this-distance this-neighbor])
                       visited
                       current-node
                       (next neighbors)
                       new-distances
                       (inc n)))))))

(defn staircase [m n]
  (flatten (map #(repeat m %) (range n))))

(defn cave-sum-inc-rotate [n]
  (inc (mod (- n 1) 9)))


(defn expand-p15-input [grid n]
  (mapv #(mapv (fn [i x] (cave-sum-inc-rotate (+ i x %2))) (take (* n (count %1)) (cycle %1)) (staircase (count %1) n))
        (take (* n (count grid)) (cycle grid)) (staircase (count grid) n)))


(defn problem15 [input]
  (let [cave-map (->> input str/trim read-heightmap)
        expanded-cave-map (expand-p15-input cave-map 5)]
    (println (dijkstra-distance cave-map [0 0] (find-end-pos cave-map)))
    (println (dijkstra-distance expanded-cave-map [0 0] (find-end-pos expanded-cave-map)))))

(def hex-to-bits
  {"0" '(0 0 0 0)
   "1" '(0 0 0 1)
   "2" '(0 0 1 0)
   "3" '(0 0 1 1)
   "4" '(0 1 0 0)
   "5" '(0 1 0 1)
   "6" '(0 1 1 0)
   "7" '(0 1 1 1)
   "8" '(1 0 0 0)
   "9" '(1 0 0 1)
   "A" '(1 0 1 0)
   "B" '(1 0 1 1)
   "C" '(1 1 0 0)
   "D" '(1 1 0 1)
   "E" '(1 1 1 0)
   "F" '(1 1 1 1)})

(defn bits-to-int-r [input-bits i]
  (if (nil? input-bits) 0
                        (+ (* i (first input-bits))
                           (bits-to-int-r (next input-bits) (* 2 i))))
  )

(defn bits-to-int [input-bits]
  (bits-to-int-r (reverse input-bits) 1))

(defrecord packet [version id sub-packets value])

(defn parse-hex-to-bits [input-hex]
  (mapcat hex-to-bits input-hex))

(defn carve-bits-to-int [n bits]
  (list (bits-to-int (take n bits)) (drop n bits)))

(defn first-is-one [bits]
  (= (first bits) 1))

(defn read-literal-bits [input-bits]
  (let [groups (partition 5 5 nil input-bits)
        result (bits-to-int
                 (concat (mapcat next (take-while first-is-one groups))
                         (next (first (drop-while first-is-one groups)))))
        rest (reduce concat (next (drop-while first-is-one groups)))]
    (list result rest)))

(defn parse-all-bits [input-bits bits-fn]
  (if (empty? input-bits) nil
                          (let [[p r] (bits-fn input-bits)]
                            (conj (parse-all-bits r bits-fn) p))))

(defn parse-n-packets [input-bits bits-fn n]
  (if (zero? n) (list '() input-bits)
                (let [[p r] (bits-fn input-bits)
                      [next-list next-rest] (parse-n-packets r bits-fn (dec n))]
                  (list (cons p next-list) next-rest))))

(defn get-operator-fns [bit-fn]
  (list
    (fn [input-bits]
      (let [[length rest] (carve-bits-to-int 15 input-bits)]
        (list (parse-all-bits (take length rest) bit-fn)
              (drop length rest))))
    (fn [input-bits]
      (let [[num-packets rest] (carve-bits-to-int 11 input-bits)]
        (parse-n-packets rest bit-fn num-packets)))))

(defn read-operator-bits [input-bits bit-fn]
  (let [[length-id rest-length-id] (carve-bits-to-int 1 input-bits)]
    ((nth (get-operator-fns bit-fn) length-id) rest-length-id)))

(defn parse-bits [input-bits]
  (let [[version rest-version-bits] (carve-bits-to-int 3 input-bits)
        [packet-id rest-id-bits] (carve-bits-to-int 3 rest-version-bits)
        [packet-result rest] (case packet-id
                               4 (let
                                   [[result rest] (read-literal-bits rest-id-bits)]
                                   (list (->packet version packet-id nil result) rest))
                               (let [[result rest] (read-operator-bits rest-id-bits parse-bits)]
                                 (list (->packet version packet-id result nil) rest)))]
    (list packet-result rest)))

(defn parse-packet [input-str]
  (let [input-bits (->> input-str str-to-chars parse-hex-to-bits)]
    (parse-bits input-bits)))

(defn version-sum [packet]
  (+ (:version packet) (sum (map version-sum (:sub-packets packet)))))

(defn bool-to-int [b] (if b 1 0))

(defn eval-packet [packet]
  (let [subpacket-values (map eval-packet (:sub-packets packet))]
    (case (:id packet)
     4 (:value packet)
     0 (sum subpacket-values)
     1 (reduce * subpacket-values)
     2 (apply min subpacket-values)
     3 (apply max subpacket-values)
     5 (bool-to-int (apply > subpacket-values))
     6 (bool-to-int (apply < subpacket-values))
     7 (bool-to-int (apply = subpacket-values)))))

(defn print-packet [p n]
  (print (str/join "" (repeat n " ")))
  (println (:id p) (:value p) (count (:sub-packets p)))
  (doall (for [pa (:sub-packets p)] (print-packet pa (+ n 2)))))

(defn problem16 [input]
  (let [input-str (str/trim input)
        [p, _] (parse-packet input-str)]
    (println (version-sum p))
    (println (eval-packet p))))

(defn problem17 [input]
  )

(defn problem18 [input])
(defn problem19 [input])
(defn problem20 [input])
(defn problem21 [input])
(defn problem22 [input])
(defn problem23 [input])
(defn problem24 [input])
(defn problem25 [input])

(let [functions [problem1 problem2 problem3 problem4
                 problem5 problem6 problem7 problem8
                 problem9 problem10 problem11 problem12
                 problem13 problem14 problem15 problem16
                 problem17 problem18 problem19 problem20
                 problem21 problem22 problem23 problem24
                 problem25]]
  (defn -main
    []
    (print "Enter problem to solve: ")
    (flush)
    (let [problemNumber (Integer/parseInt (read-line))]
      ((get functions (- problemNumber 1))
       (get-puzzle-input 2021 problemNumber)))))