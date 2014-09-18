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
  (assert (<= val range))
  (assert (>= val 0))
  (assert (>= size 1))
  (assert (<= bits size))
  (let [bit-block-size (int (Math/floor (/ size bits)))
        q (println "block size =" bit-block-size)
        spare-bits (- size (* bit-block-size bits))
        q (println "spare bits =" spare-bits)
        base (nth-root range bits)
        q (println "base =" base)
        base-power-multiples (num-as-base-power-multiples val base)
        q (println "power multiples =" base-power-multiples)
        bpm-count (count base-power-multiples)
        q (println "count =" bpm-count)
        complete-bpms (flatten
                       (conj base-power-multiples
                             (take (- bits bpm-count) (repeat 0))))]
    (flatten
     (conj
      (map #(num->single-bit-in-seq bit-block-size % base) complete-bpms)
      (zero-seq spare-bits)))))

(defn long->bit-seq
  ([^long l]
     (map #(- (long %) 48) (Long/toBinaryString l)))
  ([^long l ^long b]
     (let [bit-seq (long->bit-seq l)
           prefix-bit-count (- b (count bit-seq))]
       (assert (>= prefix-bit-count 0) "Number of bits must be enough to contain the value.")
       (flatten (conj bit-seq
                      (take prefix-bit-count (repeat 0)))))))

(defn long->bit-count
  [^long l]
  (count (Long/toBinaryString l)))


(defn int32?
  [i]
  (instance? Integer i))

(def long-max 0x7FFFFFFFFFFFFFFF)

(defn long->bit-ranges
  "Look at how many bits are required to represent the value 'l' in binary
   and splits bit count into 'n' blocks with any remainder added to the
  initial blocks.

  for example, the number 127 can be represented with 7 bits. If there are
  two blocks then the first with have 4 bits and the second with have 3.

  long->bitranges(127, 2) => (4, 3)

  'n' must be greater than zero. The number of bits required to represent
  'l' must be greater than or equal to the number of blocks 'n'."
  [^long l ^long n]
  {:pre [(> n 0)
         (>= (long->bit-count l) n)]}
  (let [range-bit-count (long->bit-count l)
        range-block-size (long (Math/floor (/ range-bit-count n)))
        remainder (- range-bit-count (* range-block-size n))]
    (map
     +
     (take n (repeat range-block-size))
     (concat (take remainder (repeat 1))
             (take (- n remainder) (repeat 0))))))

(defn split-seq-fn
  [v n]
  (let [starters (take (- (count v) 1) v)
        last-split (remove empty? (split-at n (last v)))]
    (concat starters last-split)))

(defn split-seq
  "split the sequence 's' into several smaller sequences of the sizes specified in the sequence 'r'."
  [s r]
  (assert (== (count s) (reduce + r)) "The total size of the subsequences must be the same as the size of the initial sequence.")
  (assert (reduce #(and %1 (>= %2 1)) true r) "Subelements must be of size 1 or above.")
  (assert (not= 0 (count s)) "There must be a non-empty sequence to split.")
  (reduce split-seq-fn [s] r))

(defn seq->long
  [s]
  (reduce #(+ (* %1 2) %2) 0 s))

(defn seq->max
  [s]
  (long (- (Math/pow 2 (count s)) 1)))

(defn seq->single-bit-sparse-array
  [size s]
  (println "SIZE s" size s)
  (let [val (seq->long s)
        max (seq->max s)]
    (num->single-bit-in-seq size val max)))

(defn long->sparse
  [size bits value range]
  {:pre [(instance? Long size) (instance? Long bits)
         (instance? Long value) (instance? Long range)
         (> size 0) (> bits 0)
         (<= bits size)
         (>= value 0) (<= value range)
         (>= range 1) (<= range (Long/MAX_VALUE))]}
  (let [sparse-block-size (Math/floor (/ size bits))
        sparse-starting-block-size (- size (* sparse-block-size bits))
        ranges (long->bit-ranges range bits)
        ranges-bit-count (reduce + ranges)
        value-as-bits (long->bit-seq value ranges-bit-count)
        value-split (split-seq value-as-bits ranges)
        sparses (map #(seq->single-bit-sparse-array sparse-block-size %) value-split)
        q (println "block size" sparse-block-size)
        q (println "spare bits" sparse-starting-block-size)
        q (println "ranges" ranges)
        q (println "count" ranges-bit-count)
        q (println "value" value-as-bits)
        q (println "val-r" value-split)
        q (println "sparses" sparses)]
    (flatten (concat
              (zero-seq sparse-starting-block-size)
              sparses))))
