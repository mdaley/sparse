(ns sparse.core)

(defn nth-root
  [n r]
  (Math/pow n (/ 1 r)))

(defn bit-to-set
  "Returns the bit to set in an array of bits of length size which represents
   a value's position in the range (inclusive of zero).

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
  at by multiplying range and value by size / range, making the range the same as
  the number of bits and increasing the value so it fits across the whole
  range.

   Throws AssertionError if value is outside the range, or value < 0, or range <= 0"
  [^Integer size ^Number val ^Number range]
  (assert (<= val range))
  (assert (>= val 0))
  (assert (> range 0))
  (if (>= range size)
    (let [result (->
                  (+ (* val (/ size range)) 1)
                  (Math/floor)
                  (int))]
      (if (> result size)
        size
        result))
    (bit-to-set size (* val (/ size range)) size)))

(defn zero-seq
 "Build a sequence of n zeros"
 [n]
 (repeat n 0))

(defn num->single-bit-in-seq
  "Returns an array of bits of size length with a single bit set that represents
   the position of the value in the range (inclusive of zero)."
  [^Integer size ^Number val ^Number range]
  (let [bit-to-set (bit-to-set size val range)
        starting-zeros-count (- size bit-to-set)
        ending-zeros-count (- bit-to-set 1)]
    (flatten (conj (zero-seq ending-zeros-count) '(1) (zero-seq starting-zeros-count)))))

(defn- if-empty-pad-with-value
  [v s]
  (if (empty? s) (cons v '()) s))

(defn base-powers
  "Bit like hundreds, tens and units in base 10 - the powers of a base that are
   required to express the number as the sum of multiples of the powers."
  [^Number num ^Number base]
  (assert (> base 1))
  (if-empty-pad-with-value 1 (reverse (take-while #(<= % num) (iterate (partial * base) 1)))))

(defn calc-next-prm
  "Calculate the next lower power, remainder and multiple"
  [prm base-power]
  (let [new-power (dec (:power prm))
        value (:rmdr prm)
        mult (int (Math/floor (/ value base-power)))
        rmdr (- value (* mult base-power))]
    {:power new-power
     :mult mult
     :rmdr rmdr}))

(defn- correct-near-limit
  [num base-powers]
  (let [highest-power (first base-powers)]
    (if (and (> num highest-power)
             (<= num (Math/ceil highest-power))
             (> (count base-powers) 1))
      (rest base-powers)
      base-powers)))

(defn num-as-base-power-multiples
  "Expresses a number as several multiples of powers of a base that can be summed
   to make the number (approximately, as there are no 'decimal' places)."
  [^Number num ^Number base]
  (assert (> base 1))
  (assert (>= num 0))
  (let [base-powers (base-powers num base)
        base-powers (correct-near-limit num base-powers)
        start {:power (inc (count base-powers)) ; power of the base, like HTU in base 10.
               :rmdr num                        ; remainder
               :mult 0}]                        ; multiples
    (->>
     (reductions calc-next-prm start base-powers)
     (rest)
     (map :mult)
     (if-empty-pad-with-value 0))))

(defn num->sparse-seq
  "Returns a sequence of bits of length size with a number of bits set representing
   the value within the allowable range (including zero). Note that if the number of
   possible bit combinations is lower than the number of integer values in the range,
   each bit combination will often represent more than one number."
  [^Integer size ^Integer bits ^Number val ^Number range]
  (let [bit-block-size (int (Math/floor (/ size bits)))
        q (println "bit block size =" bit-block-size)
        base (nth-root range bits)
        q (println "base =" base)
        base-power-multiples (num-as-base-power-multiples val base)
        q (println "BPMs =  " base-power-multiples)
        bpm-count (count base-power-multiples)
        complete-bpms (flatten
                       (conj base-power-multiples
                             (take (- bits bpm-count) (repeat 0))))
        q (println "CBMPS = " complete-bpms)]))
