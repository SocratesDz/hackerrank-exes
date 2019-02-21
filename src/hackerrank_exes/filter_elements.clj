(ns hackerrank-exes.filter-elements
  (:require [clojure.string :refer [split join]]))

(defn freqs [coll]
  (loop [i     0
         freqs {}]
    (if (= i (count coll))
      freqs
      (let [x (nth coll i)]
        (recur (inc i) (assoc freqs x [(inc (first (get freqs x [0 i]))) (second (get freqs x [0 i]))]))))))

(defn filter-elems [n coll]
  (let [frequency (freqs coll)
        criteria #(>= (first (second %)) n)]
    (sort-by (comp second second) (filter criteria frequency))))

(defn read-input [] (line-seq (java.io.BufferedReader. (java.io.FileReader. "input02.txt"))))
;; (defn read-input [] (line-seq (java.io.BufferedReader. *in*)))

(def test-data ["3" "9 2" "4 5 2 5 4 3 1 3 4" "9 4" "4 5 2 5 4 3 1 3 4" "10 2" "5 4 3 2 1 1 2 3 4 5"])

(defn proccess-input [lines]
  (let [lines (rest lines)
        lines (map (fn [l] (map #(Integer/parseInt %) (split l #" "))) lines)
        values (partition 2 lines)]
    (map (fn [v] [(second (first v)) (second v)]) values)))

(filter-elems 2 [4 5 2 5 4 3 1 3 4])

(let [input (read-input) ;; test-data
      input (proccess-input input)
      filtered (map (partial apply filter-elems) input)
      filtered (map #(map first %) filtered)
      filtered (map (fn [fs] (if (empty? fs) "-1" (join " " fs))) filtered)]
  (doall (map println filtered)))
