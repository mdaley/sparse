(ns sparse.core
  (require [sparse.utils :refer :all]))

(defn long->sparse
  "Turns a `value` within the range zero to `range` (inclusive) into a sparse string
  of 1s and 0s that has length `size` and `bits` number of bits set.

  Throws assertion error for various situations:

  1. Asking for a zero size sparse array.
  2. Trying to set more bits than there are in the sparse array.
  3. Asking for a range that is less than or equal to zero.
  4. Asking for a value that is not from zero to the allowed range."
  [^long size ^long bits ^long value ^long range]
  (assert (pos? size) "Size of sparse array must be greater than zero.")
  (assert (<= bits size) "Number of bits to set in sparse array must be less than or equal to the size of the sparse array.")
  (assert (>= range 1) (str "Range must be greater than zero"))
  (assert (and (>= value 0) (<= value range)) "Value must be between zero and range inclusive.")
  (let [sparse-block-size (Math/floor (/ size bits))
        sparse-starting-block-size (- size (* sparse-block-size bits))
        ranges (long->bit-ranges range bits)
        ranges-bit-count (reduce + ranges)
        value-bits (long->bitstring value ranges-bit-count)
        value-blocks (split-bitstring value-bits ranges)
        sparse-blocks (map #(bitstring->single-bit-bitstring sparse-block-size %) value-blocks)]
    (str (zeros sparse-starting-block-size) (reduce str "" sparse-blocks))))

(defn sparse->long
  [sparse ^long range]
  (assert (>= range 1) "Range must be greater than zero")
  (assert (valid-sparse? sparse) "That's not a sparse sequence")
  (let [size (count sparse)
        bits (reduce + sparse)
        sparse-block-size (Math/floor (/ size bits))
        sparse-starting-block-size (- size (* sparse-block-size bits))
        ranges (long->bit-ranges range bits)]))
