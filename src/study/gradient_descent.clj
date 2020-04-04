(ns study.gradient-descent
  (:use clojure.core.matrix
        [incanter.charts :only [scatter-plot add-lines]]
        [incanter.core   :only [view]]))

(def x [0.2 0.4 0.6 0.8 1.1 1.2 1.4 1.6 1.8 2.0 2.2 2.4 2.6 2.8 3.0 3.2 3.3 3.6 3.8 4.0])
(def y [2.3 0.6 2.5 1.5 1.8 1.0 2.6 0.4 1.4 2.8 3.5 3.2 5.5 6.6 5.2 7.0 5.5 8.0 7.4 11.4])

(def alpha              0.01)
(def iterations         150)
(def initial-theta      [0 0])
(def X (matrix (map (partial conj [1])  x)))

(defn hypothesis
  [theta X]
  (mmul theta (transpose X)))

(defn mean-square-error
  [guess actual]
  (-> (sub guess actual) square esum))

(defn compute-cost
  [X y theta]
  (let [predicted (hypothesis theta X)]
    (/ (mean-square-error predicted y)
       (* 2 number-of-examples))))

(defn cost-derivative
  [X y theta]
  (let [prediction (hypothesis theta X)]
    (-> prediction
        (sub  y)
        (mmul X)
        (mul  (/ alpha (count x))))))

(defn gradient-descent
  [X y theta alpha iterations]
  (loop [current iterations
         theta   theta
         costs   []]
    (if (zero? current)
      costs
      (recur (dec  current)
             (sub  theta (cost-derivative X y theta))
             (conj costs (compute-cost    X y theta))))))

(def cost-history (gradient-descent X y initial-theta alpha iterations))

(defn plot-cost-history
  [y]
  (view
   (scatter-plot (range iterations) y
                 :x-label "Iteration"
                 :y-label "Cost")))

(plot-cost-history cost-history)
