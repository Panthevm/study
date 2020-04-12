(ns app.utils
  (:require [uncomplicate.neanderthal.core   :as c]
            [uncomplicate.neanderthal.native :as n]
            [uncomplicate.neanderthal.math   :as m]))

(defn log
  "Logarithm of a matrix column"
  [matrix column]
  (c/alter! (c/copy matrix)
            (fn ^double [^long _ ^long *column ^double *value]
              (cond-> *value (= column *column) m/log))))

(defn vandermonde
  "Vandermonde matrix"
  [matrix]
  (c/alter! (c/copy matrix)
            (fn ^double [^long row ^long *column ^double *value]
              (m/pow (c/entry matrix row 1) *column))))

(defn ones
  "Units matrix"
  [rows coll]
  (let [*count (count coll)]
    (n/dge *count rows (concat (take *count (repeat 1)) coll))))

(defn table-print [x-data y-data results]
  (clojure.pprint/print-table
   (vector (reduce
            (fn [acc {:keys [label solve]}]
              (assoc acc (keyword label) (:coeff solve)))
            {}
            results)))
  (clojure.pprint/print-table
   [:x :y :Linear :Power :Exponential :Polynomial]
   (map
    (fn [idx a b]
      (letfn [(get-result [i]
                (nth (get-in (nth results i) [:solve :result]) idx))]
        (hash-map :x a :y b :Linear (get-result 0) :Power (get-result 1) :Exponential (get-result 2) :Polynomial (get-result 3))))
    (iterate inc 0)
    x-data
    y-data))
  results)

