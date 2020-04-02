(ns core
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.operators :as o]))

                                        ;Chapter 1
(defn square-mat
  "Creates a square matrix of size n x n
  whose elements are all e"
  [n e]
  (let [repeater (partial repeat n)]
    (m/matrix (-> e repeater repeater))))

(defn id-computed-mat
  "Creates an identity matrix of size n x n
  using compute-matrix"
  [n]
  (m/compute-matrix [n n] #(if (= %1 %2) 1 0)))

(defn rand-computed-mat
  "Creates an n x m matrix of random elements   using compute-matrix"
  [n m]
  (m/compute-matrix [n m] (fn [i j] (rand-int 100))))

(defn mat-eq
  "Checks if two matrices are equal"
  [a b]
  (and (= (count a) (count b))
       (reduce #(and %1 %2) (map = a b))))

(defn mat-add
  "Add two or more matrices"
  ([a b]
   (mapv #(mapv + %1 %2) a b))
  ([a b & more]
   (let [m (concat [a b] more)]
     (reduce mat-add m))))

(defn trace-mat [a]
  (reduce
   (fn [acc col]
     (+ acc (apply + col)))
   0
   a))

(trace-mat (square-mat 2 2))

(comment
  (square-mat 2 2)
  (id-computed-mat 2)
  (rand-computed-mat 2 2)
  (= (o/+ (square-mat 2 2)
          (square-mat 2 2))
     (mat-add (square-mat 2 2)
              (square-mat 2 2)))
  (mat-eq (square-mat 2 2) (square-mat 2 2))

  )
