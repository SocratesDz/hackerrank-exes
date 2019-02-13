(ns hackerrank-exes.polygon-area
  (:require [clojure.string :refer [split]]))

(defn square [n] (* n n))

(defn point-area-distance [pair1 pair2]
  (let [x1 (first pair1)
        x2 (first pair2)
        y1 (second pair1)
        y2 (second pair2)]
    (- (* x1 y2) (* y1 x2))))

(defn polygon-area [coords]
  (let [coords (conj (into [] coords) (first coords))]
    (Math/abs
     (/
      (reduce + (map (partial apply point-area-distance) (partition 2 1 coords)))
      2.0))))

(defn read-input [] (line-seq (java.io.BufferedReader. *in*)))

(defn process-values [input]
  (let [coords (rest input)
        coords (map #(split % #" ") coords)
        coords (map (fn [v] (map #(Integer/parseInt %) v)) coords)]
    coords))

(comment
  (let [input  (read-input)
        values (process-values input)
        area   (polygon-area values)]
    (println area)))
