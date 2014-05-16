(ns small.set-test
  (:use [clojure.test])
  (:require [small.set :as s]))

(deftest set0
  (is
   (= s/set0 #{}))

  (is
   (= (empty s/set0)
      s/set0))
  (is
   (not (contains? s/set0 0)))

  (is
   (= [] (seq s/set0)))

  (is
   (= (disj s/set0)
      s/set0))

  (is
   (= (disj s/set0 1)
      s/set0))

  (is
   (= #{1}
      (conj s/set0 1)))

  (is
   (= small.set.Set1
      (type (conj s/set0 1))))

  )
