(ns study.linear-regression
  (:use clojure.core.matrix
        [incanter.charts :only [scatter-plot add-lines]]
        [incanter.core   :only [view]]))

(def x [0.2 0.4 0.6 0.8 1.1 1.2 1.4 1.6 1.8 2.0 2.2 2.4 2.6 2.8 3.0 3.2 3.3 3.6 3.8 4.0])
(def y [2.3 0.6 2.5 1.5 1.8 1.0 2.6 0.4 1.4 2.8 3.5 3.2 5.5 6.6 5.2 7.0 5.5 8.0 7.4 11.4])

(defn linear [x y]
  (let [x   (matrix (map (partial conj [1])  x))
        xt  (transpose x)
        xtx (mmul xt x)]
    (mmul (inverse xtx) xt (matrix y))))

(defn result [x y]
  (let [[b0 b1] (vec (linear x y))]
    (map (fn [v] (+ b0 (* b1 v))) x)))

(defn plot-model []
  (view (add-lines (scatter-plot x y) x (result x y))))
