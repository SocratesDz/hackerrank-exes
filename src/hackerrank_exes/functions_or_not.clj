(ns hackerrank-exes.functions-or-not
  (:require [clojure.string :refer [join split split-lines]]))

(defn process-values [values]
  (map
   (comp (partial zipmap [:x :y]) #(split % #" "))
   values))

(defn is-valid-function? [input-values]
  (let [relations (->> (process-values input-values)
                       (group-by :x)
                       (vals)
                       ((fn [fns] (map (comp #(== (count %) 1) distinct) fns))))]
    (every? true? relations)))

(defn read-input [] (line-seq (java.io.BufferedReader. *in*)))

(defn process-inputs []
  (let [input (read-input)]
    (loop [data (rest input)
           acc  []]
      (if (empty? data)
        acc
        (let [amount    (Integer/parseInt (first data))
              relations (take amount (rest data))]
          (recur (drop (inc amount) data) (conj acc relations)))))))

(defn process-output []
  (let [input          (process-inputs)
        are-functions? (map is-valid-function? input)]
    (doseq [result are-functions?]
      (if result
        (println "YES")
        (println "NO")))))
