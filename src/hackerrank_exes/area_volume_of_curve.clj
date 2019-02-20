(ns hackerrank-exes.area-volume-of-curve
  (:require [clojure.string :refer [split]]))

(def subinterval 0.001)

(defn exp [n m] (Math/pow ^double n ^double m))

(defn dx [a b n] (/ (- b a) n))

(defn c [a b i n] (+ a (* (dx a b n) i)))

(defn formula [factor exponent]
  (fn [x] (* factor (exp x exponent))))

(defn full-formula [factors exponents]
  (let [formulae (map formula factors exponents)]
    (fn [x] (apply + (map #(% x) formulae)))))

(defn solve [factors exponents limits subinterval]
  (let [a         (first limits)
        b         (second limits)
        f         (full-formula factors exponents)
        c         (partial c a b)
        dx        (partial dx a b)
        ns        (range 1 (inc (/ (- b a) subinterval)))
        area-fn   (fn [i] (* (f (c i (count ns))) (dx (count ns))))
        volume-fn (fn [i] (* Math/PI (exp (f (c i (count ns))) 2) (dx (count ns))))]
    [(apply + (map area-fn ns)) (apply + (map volume-fn ns))]))

(defn read-input []
  (let [to-int (fn [string] (map #(Integer/parseInt %) (split string #" ")))
        lines  (line-seq (java.io.BufferedReader. *in*))]
    (vec (map to-int lines))))

(def test-data [[1 2] [0 1] [2 20]])

(let [input  (conj test-data subinterval);; (conj (read-input) subinterval)
      solved (apply solve input)]
  (doseq [n solved] (println n)))
