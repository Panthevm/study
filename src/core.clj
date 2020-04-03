(ns core
  (:use clojure.core.matrix)
  (:require [incanter.charts :refer [xy-plot add-points]]
            [incanter.core :refer [view]]))

(defn lmatrix
  [n]
  (compute-matrix [n (+ n 2)]
                  (fn [i j] ({0 -1, 1 2, 2 -1} (- j i) 0))))

(defn problem
  [n n-observed lambda]
  (let [i (shuffle (range n))]
    {:l               (mmul (lmatrix n) lambda)
     :observed        (take n-observed i)
     :hidden          (drop n-observed i)
     :observed-values (matrix (repeatedly n-observed rand))}))

(defn solve
  [{:keys [m observed hidden observed-values] :as problem}]
  (let [m1  (select m :all hidden)
        m2  (select m :all observed)
        m11 (mmul (transpose m1) m1)
        m12 (mmul (transpose m1) m2)]
    (assoc problem :hidden-values
           (mmul -1 (inverse m11) m12 observed-values))))

(defn plot-points
  [s]
  (let [x (concat (:hidden s) (:observed s))
        y (concat (:hidden-values s) (:observed-values s))]
    (view
      (add-points
        (xy-plot x y) (:observed s) (:observed-values s)))))

(defn plot-rand-sample []
  (plot-points (solve (problem 300 20 30))))
