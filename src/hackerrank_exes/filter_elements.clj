(ns hackerrank-exes.filter-elements
  (:require [clojure.string :refer [split join]]))

(defn -frequencies [coll]
  (loop [coll  coll
         freqs (array-map)]
    (if (nil? (first coll))
      freqs
      (let [x     (first coll)
            freqs (assoc freqs x (inc (get freqs x 0)))]
        (recur (rest coll) freqs)))))

(defn solve [n coll]
  (filter #(>= (second %) n) (-frequencies coll)))

(defn read-input [] (line-seq (java.io.BufferedReader. *in*)))

(defn proccess-input [lines]
  (let [lines  (rest lines)
        lines  (map (fn [l] (map #(Integer/parseInt %) (split l #" "))) lines)
        values (partition 2 lines)]
    (map (fn [v] [(second (first v)) (second v)]) values)))

(let [input    (read-input)
      input    (proccess-input input)
      filtered (map (partial apply solve) input)
      filtered (map #(map first %) filtered)
      filtered (map (fn [fs] (if (empty? fs) "-1" (join " " fs))) filtered)]
  (doall (map println filtered)))
