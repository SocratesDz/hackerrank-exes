(ns hackerrank-exes.sierpinski-triangle
  (:require [clojure.string :refer [join]]))

(def rows 32)
(def cols 63)

(defn layers [x] (Math/pow 2 x))

(defn in-range? [start end val] (or (and (>= val start) (< val end)) (= start end val)))

(defn sierpinsky-pyramid [iterations]
  (let [center           (quot cols 2)
        triangle-heights (range 0 rows (/ rows (inc iterations)))]
    (for [y (range rows)]
      (for [x (range cols)]
        (if (in-range? (- center y) (inc (+ center y)) x) \1 \_))))

(doall (map (partial apply println) (map #(apply str %) (sierpinsky-pyramid)))

;; (first (pyramid-line))

;; (last (pyramid-line))

;; (let [iterations (layers 0)
;;       line       (for [x (range cols)]
;;                    (if (= x (quot cols 2)) \1 \_))]
;;   line)
