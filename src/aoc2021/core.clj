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
        (spit cached-file download-input)
        download-input))))

(defn count-increases [coll]
  (count (filter (partial apply <)
                 (partition 2 1 coll))))

(defn moving-sum [n coll]
  (map sum (partition n 1 coll)))

(defn parse-int [s] (Integer/parseInt s))

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

(let [functions [problem1
                 problem2
                 problem3]]
  (defn -main
    "Main entry point"
    []
    (print "Enter problem to solve: ")
    (flush)
    (let [problemNumber (Integer/parseInt (read-line))]
      ((get functions (- problemNumber 1))
       (get-puzzle-input 2021 problemNumber)))))