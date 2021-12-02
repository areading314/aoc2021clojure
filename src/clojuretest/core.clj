(ns clojuretest.core)

(defn sum [coll]
  (reduce + coll))

(defn count-increases [coll]
  (count (filter (partial apply <)
                 (partition 2 1 coll))))

(defn moving-sum [n coll]
  (map sum (partition n 1 coll)))

(defn parse-int [s] (Integer/parseInt s))

(defn problem1 []
  (let [integers (->>
                   (slurp "inputs/1.txt")
                   (clojure.string/split-lines)
                   (map parse-int))]
    (println (count-increases integers))
    (println (count-increases (moving-sum 3 integers)))))

(defn parse-movement [movement-string]
  (let [[dir-string distance-string] (clojure.string/split movement-string #" ")
        distance (parse-int distance-string)]
    (case dir-string
      "forward" [0 distance]
      "backward" [0 (- distance)]
      "down" [distance 0]
      "up" [(- distance) 0])))

(defn add-movements1 [m1 m2]
  [(+ (first m1) (first m2))
   (+ (second m1) (second m2))])

(defstruct ship :h :x :aim)

(defn add-movements2 [s m]
  (struct ship (+ (get s :h) (* (second m) (get s :aim)))
          (+ (get s :x) (second m))
          (+ (get s :aim) (first m))))

(defn problem2 []
  (let [movements (->>
                    (slurp "inputs/2.txt")
                    (clojure.string/split-lines)
                    (map parse-movement))
        finalpos1 (reduce add-movements1 movements)
        finalpos2 (reduce add-movements2 (struct ship 0 0 0) movements)]
    (println (reduce * finalpos1))
    (println (* (get finalpos2 :x) (get finalpos2 :h)))))


(let [functions [problem1
                 problem2]]
  (defn -main
    "Main entry point"
    []
    (print "Enter problem to solve: ")
    (flush)
    ((get functions (- (Integer/parseInt (read-line)) 1)))))