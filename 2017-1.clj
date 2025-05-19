#!/usr/bin/env bb

(defn captcha-sum [num-str]
  (let [list (vec (map #(Character/digit % 10) num-str))
        summable-val
        (fn [i v]
          (let [n (if (>= i (- (count list) 1))
                    (first list)
                    (get list (+ i 1)))]
            (if (= v n) v 0)))]
    (reduce + (map-indexed summable-val list))))

(assert (= (captcha-sum "1122") 3))
(assert (= (captcha-sum "1111") 4))
(assert (= (captcha-sum "1234") 0))
(assert (= (captcha-sum "91212129") 9))

(let [[nums] *command-line-args*]
  (->> nums
       captcha-sum
       println))
