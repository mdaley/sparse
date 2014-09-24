(ns sparse.utils)

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
  [^long size ^long val ^long range]
  (assert (<= val range) "Value must be <= the range.")
  (assert (>= val 0) "Value must be >= zero.")
  (assert (pos? range) "Range must be > zero.")
  (if (>= range size)
    (let [result (->
                  (/ size range)
                  (* val)
                  (inc)
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
        ending-zeros-count (dec bit-to-set)]
    (flatten (conj (zero-seq ending-zeros-count) '(1) (zero-seq starting-zeros-count)))))

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
  "The number of bits required to represent the long value."
  [^long l]
  (count (Long/toBinaryString l)))

(defn long->bit-ranges
  "Look at how many bits are required to represent the value 'l' in binary
  and returns a sequence of numbers specifying the size of 'n' bit blocks
  into which the number's bits can be split. Because the number can't always
  be split into bit blocks of the same size, any additional bits are added to
  the initial blocks.

  For example, the number 127 can be represented with 7 bits. If there are
  two blocks, the first with have 4 bits and the second with have 3.

  (long->bitranges 127, 2) => (4, 3)

  If the number of bits required to represent the number is lower than 'n' the
  number of blocks requested, the ranges returned will be all of one bit size
  and there will be 'n' of them."
  [^long l ^long n]
  (assert (pos? n) "The number of blocks must be greater than zero.")
  (let [range-bit-count (long->bit-count l)
        range-block-size (long (Math/floor (/ range-bit-count n)))
        remainder (- range-bit-count (* range-block-size n))]
    (if (< range-bit-count n)
      (take n (repeat 1))
      (map +
           (take n (repeat range-block-size))
           (concat (take remainder (repeat 1))
                   (take (- n remainder) (repeat 0)))))))

(defn split-seq-fn
  "Function supporting the spliting of a sequence into smaller sequences. Expects 'v'
  a sequence of one or more nested sequences and 'n' a number of elements. 'n' is used
  to split the last nested sequence in the overall sequence 'v' and then the larger
  sequence of nested sequences is returned. When called using reduce, a sequence can
  be split into parts of the sizes specified in the sequence containing the values of 'n'."
  [v n]
  (let [starters (take (dec (count v)) v)
        last-split (remove empty? (split-at n (last v)))]
    (concat starters last-split)))

(defn split-seq
  "Split the sequence 's' into several smaller sequences of the sizes specified in the sequence 'r'."
  [s r]
  (assert (== (count s) (reduce + r)) "The total size of the sub-sequences must be the same as the size of the initial sequence.")
  (assert (reduce #(and %1 (>= %2 1)) true r) "Subelements must be of size 1 or above.")
  (assert (not= 0 (count s)) "There must be a non-empty sequence to split.")
  (reduce split-seq-fn [s] r))

(defn seq->long
  [s]
  (reduce #(+ (* %1 2) %2) 0 s))

(defn seq->max
  [s]
  (long (dec (Math/pow 2 (count s)))))

(defn seq->single-bit-sparse-array
  "Turn a sequence of 1s and 0s into a sparse array with only one bit set."
  [size s]
  (let [val (seq->long s)
        max (seq->max s)]
    (num->single-bit-in-seq size val max)))


(defn valid-sparse?
  "Check that a sparse sequence is valid, i.e. only contains 1s and 0s or is empty."
  [sparse]
  (not (some #(not (or (= 1 %) (= 0 %))) sparse)))
