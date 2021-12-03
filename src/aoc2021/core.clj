(ns aoc2021.core
  (:require [clj-http.client :as client]))

(defn sum [coll]
  (reduce + coll))

(defn get-puzzle-input [year puzzle-number]
  (let [session-cookie (format "session=%s" (slurp (System/getenv "AOC_COOKIE_FILE")))]
    (get (client/get
           (format "https://adventofcode.com/%s/day/%s/input" year puzzle-number)
           {:headers {:cookie session-cookie}})
         :body)))

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
  (vec (map (fn [cc aa] (if (= aa "1") (+ cc 1) cc)) c a)))


(defn most-common [arrs tiebreaker op]
  (let [total-count (count arrs)
        one-count (reduce combine-bin-array (vec (repeat (count (get arrs 0)) 0)) arrs)
        zero-count (vec (map #(- total-count %) one-count))]
    (vec (map (fn [zc oc] (cond (= zc oc) tiebreaker
                                (op zc oc) "0"
                                (op oc zc) "1")) zero-count one-count))))

(defn find-best-prefix-match [arrs idx tiebreaker op]
  (if (<= (count arrs) 1)
    (get arrs 0)
    (let [mc (most-common arrs tiebreaker op)
          newarrs (vec (filter #(= (get % idx) (get mc idx)) arrs))]
      (find-best-prefix-match newarrs (+ idx 1) tiebreaker op))))

(def arr-to-int (comp bin-to-int #(clojure.string/join "" %)))

(defn calc-epsgamma [inputs]
  (let [arrs (vec (map #(clojure.string/split % #"") inputs))
        gamma (arr-to-int (most-common arrs "1" >))
        epsilon (arr-to-int (most-common arrs "0" <))
        oxygen-rating (arr-to-int (find-best-prefix-match arrs 0 "1" >))
        co2-scrubber-rating (arr-to-int (find-best-prefix-match arrs 0 "0" <))]
    (println (* gamma epsilon))
    (println (* oxygen-rating co2-scrubber-rating)))
  )


(defn problem3 [input]
  (->> input (clojure.string/split-lines) calc-epsgamma))


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