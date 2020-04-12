(ns app.linear
  (:require [uncomplicate.neanderthal.core   :as c]
            [uncomplicate.neanderthal.native :as n]
            [uncomplicate.neanderthal.math   :as m]
            [uncomplicate.neanderthal.linalg :as l]
            [app.utils                       :as u]
            [com.hypirion.clj-xchart         :as chart]))

(def ^:const x-data [0.2 0.4 0.6 0.8 1.1 1.2 1.4 1.6 1.8 2.0 2.2 2.4 2.6 2.8 3.0 3.2 3.3 3.6 3.8 4.0])
(def ^:const y-data [2.3 0.6 2.5 1.5 1.8 1.0 2.6 0.4 1.4 2.8 3.5 3.2 5.5 6.6 5.2 7.0 5.5 8.0 7.4 11.4])
(def ^:const *count (count x-data))
(def ^:const degree 3)

(def *x  (u/ones 2 x-data))
(def *y  (n/dge *count 1 y-data))
(def *xd (u/ones degree x-data))

(defn linear [x y]
  (let [xt  (c/trans x)
        xtx (c/mm xt x)
        xty (c/mm xt y)]
    (-> xtx l/trf l/tri (c/mm xty))))

(defn result-linear [x y]
  (letfn [(formula [a b x]
            (+ a (* b x)))]
    (let [[[a b]] (seq (linear x y))]
      {:result (map (partial formula a b) x-data)
       :coeff  [a b]})))

(defn result-pow [x y]
  (letfn [(formula [a b x]
            (* (m/exp a) (m/pow x b)))]
    (let [[[a b]] (seq (linear (u/log x 1) (u/log y 0)))]
      {:result (map (partial formula a b) x-data)
       :coeff  [(m/exp a) b]})))

(defn result-exp [x y]
  (letfn [(formula [a b x]
            (m/exp (+ a (* b x))))]
    (let [[[a b]] (seq (linear x (u/log y 0)))]
      {:result (map (partial formula a b) x-data)
       :coeff [a b]})))


(defn result-pol [x y]
  (let [xv    (u/vandermonde x)
        coeff (linear xv y)]
    {:result (first (seq (c/mm xv coeff)))
     :coeff  (vec (first (seq coeff)))}))

(defn chart [x y & [results]]
  (chart/view
   (chart/xy-chart (reduce
                    (fn [acc {:keys [solve label]}]
                      (assoc acc label {:x     x-data
                                        :y     (:result solve)
                                        :style {:render-style :line
                                                :marker-type  :none}}))
                    {"Data" [x-data y-data]} results)
                   {:x-axis       {:title "X"}
                    :y-axis       {:title "Y"}
                    :render-style :scatter
                    :theme        :matlab})))

(time (->> [{:label "Linear      regression" :solve (result-linear *x  *y)}
            {:label "Power       regression" :solve (result-pow    *x  *y)}
            {:label "Exponential regression" :solve (result-exp    *x  *y)}
            {:label "Polynomial  regression" :solve (result-pol    *xd *y)}]
           (u/table-print x-data y-data)
           (chart x y)))
