(ns hackerrank-exes.polygon-perimeter
  (:require [clojure.string :refer [split]]))

(defn square [n] (* n n))

(defn point-distance [pair1 pair2]
  (let [x1 (first pair1)
        x2 (first pair2)
        y1 (second pair1)
        y2 (second pair2)]
    (Math/sqrt (+ (square (- x2 x1)) (square (- y2 y1))))))

(defn polygon-perimeter [coords]
  (let [coords (conj (into [] coords) (first coords))]
    (reduce + (map (partial apply point-distance) (partition 2 1 coords)))))

(defn read-input [] (line-seq (java.io.BufferedReader. *in*)))

(defn process-values [input]
  (let [coords (rest input)
        coords (map #(split % #" ") coords)
        coords (map (fn [v] (map #(Integer/parseInt %) v)) coords)]
    coords))

(comment
  (let [input     (read-input)
        values    (process-values input)
        perimeter (polygon-perimeter values)]
    (println perimeter)))
