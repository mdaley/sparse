(ns sparse.core)

(defn nth-root
  [n r]
  (Math/pow n (/ 1 r)))

(defn bit-to-set
  "Returns the bit to set in an array of bits of size n which represents
   the value v's position in the range zero to r.

   For example if the number of bits is 10 and the range is 99, the value
   0 has the bit position 0, hence the bit array would be 0000000001.

   If the value is 99 the bit position is 10, hence the bit array is 1000000000.

   If the value is 33, the bit position is 4, hence the bit array is 0000001000.

   Don't forget that several numbers, along with zero, can be represented by
   bit position 0.

   Of course if the number range is large and the bit array is small, then many
   numbers will be represented by each bit position. If the number range is the
   same as the number of bits, e.g. the number of bits is 100 and the number range
   is (zero to) 99, then one bit represents each number - bit 14 represents the
   number 13, and bit 1 represents the number zero.

  If the number range is less than the number of bits, the value is arrived
  at by multiplying range and value by n / r, making the range the same as
  the number of bits and increasing the value so it fits across the whole
  range.

   Throws AssertionError if v is outside the range r, or v < 0, or r <= 0"
  [^Integer n ^Number v ^Number r]
  (assert (<= v r))
  (assert (>= v 0))
  (assert (> r 0))
  (if (>= r n)
    (let [result (->
                  (+ (* v (/ n r)) 1)
                  (Math/floor)
                  (int))]
      (if (> result n)
        n
        result))
    (bit-to-set n (* v (/ n r)) n)))

(defn zero-seq
 "Build a sequence of n zeros"
 [n]
 (repeat n 0))

(defn num->single-bit-in-seq
  "Returns an array of bits of size n with a single bit set that represents
   the position of the value v in the range (zero to) r (inclusive)."
  [^Integer n ^Number v ^Number r]
  (let [bit-to-set (bit-to-set n v r)
        starting-zeros-count (- n bit-to-set)
        ending-zeros-count (- bit-to-set 1)]
    (flatten (conj (zero-seq ending-zeros-count) '(1) (zero-seq starting-zeros-count)))))

(defn base-powers
  "Bit like hundreds, tens and units - the powers of the base b that are required to
   express the number n as the sum of multiples of the powers."
  [^Number n ^Number b]
  (reverse (take-while #(<= % n) (iterate (partial * b) 1))))

(defn num-as-base-power-multiples
  "Expresses the number n as several multiples of powers of the base b that can be summed
   to make the number (approximately, as there are no 'decimal' places."
  [^Integer n ^Number b]
  (let [bp (base-powers n b)
        start {:p (inc (count bp)) ;power of the base, like index of HTU in base 10.
               :r n                ; remainder
               :c 0}]              ; count
    (reductions (fn [v p]
                  (let [n (dec (:p v))
                        v (:r v)
                        c (Math/floor (/ v p))
                        r (- v (* c p))]
                    {:p n
                     :c c
                     :r r}))
                start
                bp)))

(defn num->sparse-seq
  "Returns a sequence of bits of size n with b bits set that represents the value
   v within the allowable range (zero to) r. Note that if the number of possible
   bit combinations is lower than the number of integer values in the range, each
   bit combination will often represent more than one number."
  [^Integer n ^Integer b ^Number v ^Number r]
  (let [bit-block-size (->
                        (/ b n)
                        (Math/floor)
                        (int))
        base (nth-root r b)]))
