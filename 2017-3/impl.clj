#!/usr/bin/env bb

(require
 '[clojure.math :refer [pow sqrt floor]])

(defn assert-eq
  [a b]
  (assert (= a b)))

(defn between
  [min max x]
  (and (<= x max)
       (>= x min)))

(defn nearest-odd-beneath
  [n]
  (if (= 0 (mod n 2))
    (- n 1)
    n))

(defn neg
  [n]
  (* -1 n))

(defn layer
  "Distance from origin for spiral"
  [idx]
  (-> idx
      (- 1) ;; 1-indexed
      sqrt
      floor
      nearest-odd-beneath
      (+ 1)
      (/ 2)
      int))

(defn coord
  [idx]
  (if (= idx 1)
    [0 0]
    (let [layer (layer idx) ;; shadow... is OK?
          layer-len (* layer 8)
          layer-start-idx (-> layer
                              (* 2)
                              (- 1)
                              (pow 2)
                              int)
          l-offset (-> idx
                       (- layer-start-idx)
                       (mod layer-len))
          quad-len (quot layer-len 4)
          quadrant (quot l-offset quad-len)
          quad-offset (mod l-offset quad-len)]
      (case quadrant
        0 [layer (- quad-offset layer)] ;; right side
        1 [(- layer quad-offset) layer] ;; top side
        2 [(neg layer) (- layer quad-offset)] ;; left side
        3 [(- quad-offset layer) (neg layer)] ;; bottom side
        ))))

(defn path-len
  [pair]
  (reduce + (map abs pair)))

(defn spiral-path
  "Does the thing"
  [input]
  (-> input
      Integer/parseInt
      coord
      path-len))

(defn spiral-key
  [coord]
  (str/join "x" coord))

(defn neighbors
  [[x y]]
  (for [xd (range -1 2)
        yd (range -1 2)]
    [(+ x xd) (+ y yd)]))

(defn spiral-sum-val
  "Calculate value at coordinate"
  [coord spiral]
  (if (= coord [0 0])
    1
    (reduce + (map #(get spiral (spiral-key %) 0) (neighbors coord)))))

(defn spiral-sum
  "Dynamically build up until max is surpassed, adding neighbors"
  ([max]
   (spiral-sum max 1 {}))
  ([max coord-idx spiral]
   (let [coord (coord coord-idx)
         next (spiral-sum-val coord spiral)]
     (println (str/join " " [coord next]))
     (if (< max next)
       next
       (spiral-sum max (+ 1 coord-idx) (assoc spiral (spiral-key coord) next))))))


(-> *in*
    slurp
    spiral-path
    println)

;;; NOTES


;; (assert-eq (spiral-path "1") 0)
;; (assert-eq (spiral-path "12") 3)
;; (assert-eq (spiral-path "23") 2)
;; (assert-eq (spiral-path "1024") 31)


;; (assert-eq (nearest-odd-beneath 6) 5)
;; (assert-eq (nearest-odd-beneath 7) 7)
;; (assert-eq (nearest-odd-beneath 8) 7)
;; (assert-eq (nearest-odd-beneath 9) 9)

;; 1  - 1 add slot [0 0] -- range = 0, idx = 0
;; 2 [[1 0]]             -- range = 1, idx = 0, range-idx = 1
;; 3 [[1 1]]             -- range = 1, idx = 1, range-idx = 2
;; 4 [[0 1]]
;; 9  - 8 add slots         range = 2 3^2 length = 16
;; 25 - 16 add slots        range = 3 5^2 length = 24
;; 49 - 24 add slots        range = 4 7^2
;; 81 - 32 add slots? 8*4 + 8*2 + 8*1 + 1 range = 5  9^2
;; ...
;; SUM OF SQUARES?

;; idx -> -1 -> sqrt -> floor -> nearest lower odd number -> +1 -> /2 = range!


;; (between 0 5 3) => true
;; (between 0 5 6) => false
;; (between 0 5 5) => true

;; (assert-eq (range 1) 0)
;; (assert-eq (range 2) 1)
;; (assert-eq (range 24) 2)
;; (assert-eq (range 25) 2)
;; (assert-eq (range 26) 3)
;; (assert-eq (range 80) 4)
;; (assert-eq (range 81) 4)
;; (assert-eq (range 82) 5)
;; (assert-eq (range 83) 5)


;; Range algo (range, idx)
;;   use range-idx = idx + 1 for everything to "shift" (and idx = max for range is treated as 0)
;;   if range-idx between 0 and range / 4
;;      x = range
;;      y = (-range + range-idx)
;;   if range-idx between range / 4 and 2*range/4
;;      x = (-range + range-idx)
;;      y = -range
;; ...
