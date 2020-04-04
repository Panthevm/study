(ns interpolating.core
  (:use clojure.core.matrix)
  (:require [incanter.charts :refer [xy-plot add-points]]
            [incanter.core :refer [view]]))

(defn lmatrix
  [n]
  (compute-matrix [n (+ n 2)]
                  (fn [i j] ({0 -1, 1 2, 2 -1} (- j i) 0))))

(defn problem
  [n n-observed lambda]
  (let [m (shuffle (range n))]
    {:m               (mmul (lmatrix n) lambda)
     :observed        (take n-observed m)
     :hidden          (drop n-observed m)
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
  [{:keys [hidden observed hidden-values observed-values]}]
  (let [x (concat hidden observed)
        y (concat hidden-values observed-values)]
    (view
      (add-points
       (xy-plot x y) observed observed-values))))

(defn plot-rand-sample []
  (plot-points (solve (problem 300 20 30))))
