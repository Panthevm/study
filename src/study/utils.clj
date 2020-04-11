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

