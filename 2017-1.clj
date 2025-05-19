(defn captcha-sum [list]
  (let [summable-val
        (fn [i v]
          (let [n (if (>= i (- (count list) 1))
                    (first list)
                    (get list (+ i 1)))]
            (if (= v n) v 0)))]
    (reduce + (map-indexed summable-val list))))

(assert (= (captcha-sum [1 1 2 2]) 3))
(assert (= (captcha-sum [1 1 1 1]) 4))
(assert (= (captcha-sum [1 2 3 4]) 0))
(assert (= (captcha-sum [9 1 2 1 2 1 2 9]) 9))
