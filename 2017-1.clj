#!/usr/bin/env bb

(defn captcha-sum-next [num-str]
  (let [list (vec (map #(Character/digit % 10) num-str))
        summable-val
        (fn [i v]
          (let [n (if (>= i (- (count list) 1))
                    (first list)
                    (get list (+ i 1)))]
            (if (= v n) v 0)))]
    (reduce + (map-indexed summable-val list))))

(assert (= (captcha-sum-next "1122") 3))
(assert (= (captcha-sum-next "1111") 4))
(assert (= (captcha-sum-next "1234") 0))
(assert (= (captcha-sum-next "91212129") 9))

(defn captcha-sum-rot [num-str]
  (let [list (vec (map #(Character/digit % 10) num-str))
        list-len (count list)
        summable-val
        (fn [i v]
          (let [n (get list (mod (+ i (/ list-len 2)) list-len))]
            (if (= v n) v 0)))]
    (reduce + (map-indexed summable-val list))))

(assert (= (captcha-sum-rot "1212") 6))
(assert (= (captcha-sum-rot "1221") 0))
(assert (= (captcha-sum-rot "123425") 4))
(assert (= (captcha-sum-rot "123123") 12))
(assert (= (captcha-sum-rot "12131415") 4))

(let [[nums] *command-line-args*]
  (->> nums
       captcha-sum-rot
       println))
