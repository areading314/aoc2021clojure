(ns clojuretest.core)

(defn zip [& colls]
  (apply map vector colls))

(defn sum [coll]
  (reduce + coll))

(defn count-increases [coll]
  (reduce + (map (fn [x y] (if (< x y) 1 0))
                 (drop-last 1 coll)
                 (drop 1 coll))))

(defn measure-3 [coll]
  (map sum (zip (drop-last 2 coll)
                (drop-last 1 (drop 1 coll))
                (drop 2 coll))))

(defn problem1 []
  (let [integers (vec (->>
                        (slurp "inputs/1.txt")
                        (clojure.string/split-lines)
                        (map #(Integer/parseInt %))))]
    (println (count-increases integers))
    (println (count-increases (measure-3 integers)))
    )
  )

(let [functions [problem1]]
  (defn -main
    "Main entry point"
    []
    (println ((get functions (- (Integer/parseInt (read-line)) 1))))))