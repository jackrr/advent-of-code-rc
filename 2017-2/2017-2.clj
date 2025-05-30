#!/usr/bin/env bb

(defn long-str [& strings] (str/join "\n" strings))

(def example (long-str "5 1 9 5"
                       "7 5 3"
                       "2 4 6 8"))

(def example-div (long-str "5 9 2 8"
                            "9 4 7 3"
                            "3 8 6 5"))

(defn process-line-diff [line]
  (let [nums (map Integer/parseInt (str/split line #"\s"))]
    (- (apply max nums) (apply min nums))))

(defn process-line-div [line]
  ;; Find the two nums that evenly divide
  ;; Iterate list -- look at all ahead in list
  (let [nums (vec (map Integer/parseInt (str/split line #"\s")))]
    (first
     (keep-indexed
      (fn [idx n]
        (let [rest (subvec (vec nums) (+ idx 1))]
          (first
           (keep (fn [m]
                   (cond
                     (= (mod m n) 0) (/ m n)
                     (= (mod n m) 0) (/ n m)
                     :else nil))
                 rest))))
      nums))))

(defn checksum [data handle-line]
  (->> (str/split data #"\n")
       (map handle-line)
       (reduce +)))

(assert (= (checksum example process-line-diff) 18))
(assert (= (checksum example-div process-line-div) 9))

(-> *in*
    slurp
    (checksum process-line-div)
    println)
