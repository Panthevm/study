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

