(ns hackerrank-exes.area-volume-of-curve
  (:require [clojure.string :refer [split]]))

(def subinterval 0.001)

(defn exp [n m] (Math/pow ^double n ^double m))

(defn formula [factor exponent]
  (fn [x] (* factor (exp x exponent))))

(defn full-formula [factors exponents]
  (let [formulae (map formula factors exponents)]
    (fn [x] (apply + (map #(% x) formulae)))))

(defn solve [factors exponents limits subinterval]
  (let [form   (full-formula factors exponents)
        a      (first limits)
        b      (second limits)
        n      (range a (+ b subinterval) subinterval)
        area   (* subinterval (apply + (map form n)))
        volume (* subinterval (apply + (map #(* Math/PI (exp (form %) 2)) n)))]
    [area volume]))

(defn read-input []
  (let [to-int (fn [string] (map #(Integer/parseInt %) (split string #" ")))
        lines  (line-seq (java.io.BufferedReader. *in*))]
    (vec (map to-int lines))))

(let [input  (conj (read-input) subinterval)
      solved (apply solve2 input)]
  (doseq [n solved] (println n)))
