(ns app.linear
  (:require [uncomplicate.neanderthal.core   :as c]
            [uncomplicate.neanderthal.native :as n]
            [uncomplicate.neanderthal.linalg :as l]
            [uncomplicate.neanderthal.math   :as m]
            [com.hypirion.clj-xchart         :as chart]))

(def ^:const x-data [0.2 0.4 0.6 0.8 1.1 1.2 1.4 1.6 1.8 2.0 2.2 2.4 2.6 2.8 3.0 3.2 3.3 3.6 3.8 4.0])
(def ^:const y-data [2.3 0.6 2.5 1.5 1.8 1.0 2.6 0.4 1.4 2.8 3.5 3.2 5.5 6.6 5.2 7.0 5.5 8.0 7.4 11.4])
(def ^:const *count (count x-data))

(def x (n/dge *count 2 (concat (take *count (repeat 1)) x-data)))
(def y (n/dge *count 1 y-data))

(defn linear [x y]
  (let [xt  (c/trans x)
        xtx (c/mm xt x)
        xty (c/mm xt y)]
    (-> xtx l/trf l/tri (c/mm xty))))

(defn result-linear [x y]
  (letfn [(formula [a b x]
            (+ a (* b x)))]
    (let [[[a b]] (seq (linear x y))]
      (map (partial formula a b) x-data))))

(defn result-pow [x y]
  (letfn [(formula [a b x]
            (* (m/exp a) (m/pow x b)))
          (matrix-log [m tcol]
            (c/alter! (c/copy m)
                      (fn ^double [^long _ ^long col ^double x]
                        (cond-> x (= tcol col) m/log))))]
    (let [X (matrix-log x 1)
          Y (matrix-log y 0)
          [[a b]] (seq (linear X Y))]
      (map (partial formula a b) x-data))))

(defn chart [x y & results]
  (chart/view
   (chart/xy-chart (reduce
                    (fn [acc {:keys [y label]}]
                      (assoc acc label {:x     x-data
                                        :y     y
                                        :style {:render-style :line
                                                :marker-type  :none}}))
                    {"Данные" [x-data y-data]} results)
                   {:title        "Линейная аппроксимация"
                    :x-axis       {:title "X"}
                    :y-axis       {:title "Y"}
                    :render-style :scatter
                    :theme        :matlab})))

(time (chart x y
             {:label "label"
              :y (result-linear x y)}
             {:label "s"
              :y (result-pow x y)}))
