(ns hackerrank-exes.sequence-full-of-colors)

(defn read-input [] (line-seq (java.io.BufferedReader. *in*)))

(defn prefixers [coll]
  (map #(take % coll) (range 1 (count coll))))

(defn process-input [input]
  (let [fn-converter (fn [c] (case c \R :R \G :G \Y :Y \B :B))
        values       (rest input)
        values       (map #(map fn-converter %) values)]
    values))

(defn solve [input]
  (let [values  (frequencies input)
        fn-diff (fn [n] (and (<= (Math/abs (- (get n :R 0) (get n :G 0))) 1)
                             (<= (Math/abs (- (get n :Y 0) (get n :B 0))) 1)))]
    (and
     (= (get values :R 0) (get values :G 0))
     (= (get values :Y 0) (get values :B 0))
     (let [prefixes (prefixers input)
           prefixes (map frequencies prefixes)]
       (apply (every-pred fn-diff) prefixes)))))

(time (let [input  (read-input)
            values (process-input input)
            solved (map solve values)]
        (doall (map #(println (if % "True" "False")) solved))))
