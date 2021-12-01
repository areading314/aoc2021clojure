(ns clojuretest.core)

(defn sum [coll]
  (reduce + coll))

(defn count-increases [coll]
  (count (filter (partial apply <)
                 (partition 2 1 coll))))

(defn moving-sum [n coll]
  (map sum (partition n 1 coll)))

(defn problem1 []
  (let [integers (vec (->>
                        (slurp "inputs/1.txt")
                        (clojure.string/split-lines)
                        (map #(Integer/parseInt %))))]
    (println (count-increases integers))
    (println (count-increases (moving-sum 3 integers)))))

(let [functions [problem1]]
  (defn -main
    "Main entry point"
    []
    (println "Enter problem to solve: ")
    ((get functions (- (Integer/parseInt (read-line)) 1)))))