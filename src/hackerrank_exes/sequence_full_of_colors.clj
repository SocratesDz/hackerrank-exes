(ns hackerrank-exes.sequence-full-of-colors)

;; (defn read-input [] (line-seq (java.io.BufferedReader. *in*)))
(defn read-input [] (line-seq (java.io.BufferedReader. (java.io.FileReader. "input06.txt"))))

(defn abs [x] (if (< x 0) (* x -1) x))

(defn solve [full-string]
  (let [fn-diff (fn [balls-count] (and (<= (abs (- (balls-count :R 0) (balls-count :G 0))) 1)
                                      (<= (abs (- (balls-count :Y 0) (balls-count :B 0))) 1)))]
    (loop [string full-string
           c      (first string)
           balls  {:R 0 :G 0 :Y 0 :B 0}]
      (if (nil? c)
        (and
         (= (balls :R 0) (balls :G 0))
         (= (balls :Y 0) (balls :B 0)))
        (if-not (fn-diff balls)
          false
          (let [balls (assoc balls
                             (keyword (str c))
                             (inc ((keyword (str c)) balls)))]
            (recur (rest string) (second string) balls)))))))

(time (let [input  (rest (read-input))
            solved (map solve input)]
        (doall (map #(println (if % "True" "False")) solved))))
