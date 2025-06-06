#!/usr/bin/env bb

(defn assert-eq
  [a b]
  (assert (= a b)))

(defn thing
  "Does the thing"
  [input]
  input)

(-> *in*
    slurp
    thing
    println)
