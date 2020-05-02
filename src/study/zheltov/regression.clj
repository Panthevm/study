(ns study.zheltov.regression
  (:use [clojure.core.matrix     :as m]
        [com.hypirion.clj-xchart :as c]))

(def ^:const data
  [[0.2 0.4 0.6 0.8 1.1 1.2 1.4 1.6 1.8 2.0 2.2 2.4 2.6 2.8 3.0 3.2 3.3 3.6 3.8 4.0]
   [2.3 0.6 2.5 1.5 1.8 1.0 2.6 0.4 1.4 2.8 3.5 3.2 5.5 6.6 5.2 7.0 5.5 8.0 7.4 11.4]])

(defmulti result :type)

(defstruct answer :new-y :coeff)

(defn calc [[x y]]
  (let [x   (m/matrix (map (partial conj [1])  x))
        xt  (m/transpose x)
        xtx (m/mmul xt x)]
    (vec (m/mmul (m/inverse xtx) xt (m/matrix y)))))

(defmethod result :linear
  [{[x :as data] :data}]
  (let [[a b :as coeff] (calc data)]
    (letfn [(formula [x] (+ a (* b x)))]
      (struct answer (map formula x) coeff))))

(defmethod result :pow
  [{[x :as data] :data}]
  (let [[a b :as coeff] (calc (m/log data))]
    (letfn [(formula [x] (* (m/exp a) (m/pow x b)))]
      (struct answer (map formula x) coeff))))

(defmethod result :exp
  [{[x :as data] :data}]
  (let [[a b :as coeff] (calc (update data 1 m/log))]
    (letfn [(formula [x] (m/exp (+ a (* b x))))]
      (struct answer (map formula x) coeff))))

(defmethod result :pol ;TODO
  [{[x y :as data] :data}]
  (letfn [(vandermonde [row]
            (vec (map-indexed
                  (fn [idx v] (m/pow v idx)) row)))]
    (let [*x    (vandermonde x)
          coeff (calc (assoc data 0 *x))]
      (prn *x "@" coeff)
      (struct answer (m/mmul *x coeff) coeff))))

(defn chart [data results]
  (let [[x y] data]
    (c/view
     (c/xy-chart (reduce
                  (fn [acc {:keys [solve label]}]
                    (assoc acc label {:x     x
                                      :y     (:new-y solve)
                                      :style {:render-style :line
                                              :marker-type  :none}}))
                  {"Data" data} results)
                 {:x-axis       {:title "X"}
                  :y-axis       {:title "Y"}
                  :render-style :scatter
                  :theme        :matlab}))))

(time (->> [{:label "Linear regression"      :solve (result {:type :linear :data data})}
            {:label "Power  regression"      :solve (result {:type :pow    :data data})}
            {:label "Exponential regression" :solve (result {:type :exp    :data data})}
            #_{:label "Polynomial  regression" :solve (result {:type :pol    :data data})}]
           (chart data)))


