(ns sparse.core
  (require [sparse.utils :refer :all]))

(defn long->sparse
  [^long size ^long bits ^long value ^long range]
  (assert (pos? size) "Size of sparse array must be greater than zero.")
  (assert (<= bits size) "Number of bits to set in sparse array must be less than or equal to the size of the sparse array.")
  (assert (>= range 1) (str "Range must be greater than zero"))
  (assert (and (>= value 0) (<= value range)) "Value must be between zero and range inclusive.")
  (let [sparse-block-size (Math/floor (/ size bits))
        sparse-starting-block-size (- size (* sparse-block-size bits))
        ranges (long->bit-ranges range bits)
        ranges-bit-count (reduce + ranges)
        value-as-bits (long->bit-seq value ranges-bit-count)
        value-in-blocks (split-seq value-as-bits ranges)
        sparse-blocks (map #(seq->single-bit-sparse-array sparse-block-size %) value-in-blocks)]
    (flatten (concat
              (zero-seq sparse-starting-block-size)
              sparse-blocks))))
