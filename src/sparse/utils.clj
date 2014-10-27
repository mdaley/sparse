(ns sparse.utils
  (:require [clojure.string :refer [join]]))

(defn bit-to-set
  "Returns the bit to set in an array of bits of length `size` which represents
   the value `val`'s position in the range zero to `range`.

   For example if the number of bits is 10 and the range is 99, the value
   0 has the bit position 0, representing the bit array 0000000001.

   If the value is 99 the bit position is 9, representing 1000000000.

   If the value is 33, the bit position is 3, representing 0000001000..

   Of course if the number range is large and the bit array is small, many
   numbers will be represented by each bit position. If the number range is the
   same as the number of bits, e.g. the number of bits is 100 and the number range
   is (zero to) 99, then one bit represents each number - bit 13 represents the
   number 13, and bit 0 represents the number zero.

   If the number range is less than the number of bits, the value is arrived
   at by multiplying range and value by size / range, making the range the same as
   the number of bits and increasing the value so it fits across the whole
   range.

   Throws AssertionError if value is outside the range, or value < 0, or range <= 0"
  [^long size val range]
  (assert (<= val range) "Value must be <= the range.")
  (assert (>= val 0) "Value must be >= zero.")
  (assert (pos? range) "Range must be > zero.")
  (if (>= range size)
    (let [max-bit (dec size)
          result (->
                  (/ val range)
                  (* size)
                  (Math/floor)
                  (int))]
      (if (> result max-bit) ; at limit result will be too high.
        max-bit
        result))
    (bit-to-set size (* val (/ size range)) size)))

(defn zeros
  "Create a string of zeros of length `n`."
  [n]
  (reduce str "" (repeat n "0")))

(defn bitstring->bit-to-set
  [size bitstring]
  (let [val (Long/parseLong bitstring 2)
        range (Long/parseLong (reduce str (repeat (count bitstring) "1")) 2)]
    (bit-to-set size val range)))

(defn bits->binarystring
  [size bits]
  (reduce (fn [s x] (str s (if (contains? bits x) "1" "0"))) "" (reverse (range size))))

(defn long->bitstring
  "Turn a long value 'l' into a string of bits. In the second form, the number of bits 'b'
  can be specified with zero padding occurring where necessary. The number of bits 'b' must
  be sufficient to fit the number."
  ([^long l]
     (Long/toBinaryString l))
  ([^long l ^long b]
     (let [bits (long->bitstring l)
           prefix-bit-count (- b (count bits))]
       (assert (>= prefix-bit-count 0) "Number of bits must be enough to contain the value.")

       (str (join (take prefix-bit-count (repeat "0")))
            bits))))

(defn- long->bit-count
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

(defn- split-bitstring-fn
  "Function supports the splitting of a string into smaller strings. 'v' is a sequence
  of one or more strings where the last string is to be split into two strings, the first
  of length 'n'. The result returned is a sequence idential to the original sequence but
  with the last item split into two parts."
  [v n]
  (let [starters (take (dec (count v)) v)
        last (last v)
        last-1 (subs last 0 n)
        last-2 (subs last n)]
    (flatten (conj '() last-2 last-1 starters))))

(defn split-bitstring
  "Split the bitstring 's' into a sequence of bitstrings of sizes defined in ranges 'r'."
  [s r]
  (assert (== (count s) (reduce + r)) "The total size of the sub-bitstrings must be the same as the size of the initial bitstring")
  (assert (reduce #(and %1 (>= %2 1)) true r) "Sub-bitstrings must be of size 1 or above.")
  (assert (not= 0 (count s)) "There must be a non-empty bitstring to split.")
  (reduce split-bitstring-fn [s] (drop-last r)))

(defn valid-sparse?
  "Check that a sparse sequence is valid, i.e. only contains 1s and 0s or is empty."
  [sparse]
  (not-any? #(not (or (= 1 %) (= 0 %))) sparse))

; Go from a sparse bit sequence back to the original number (or a possible range as
; the answer often won't be precise).

(defn bit-pos->num
  "Turn the position of a bit in a particular size sequence into the original value, or
   more precisely into three values: the highest possible value that could be represented
  by this bit position, the mid-value and the lowest possible value.

  Suppose that an 8 bit sparse array represents a value in the range zero to 100,000.
  Fairly obviously, one bit represents a large range of values. In this case, the
  sparse sequence (1 0 0 0 0 0 0 0) can have been generated from numbers from 100,000
  down to 87,500 and the mid-point value is 93,750.

  So, sparse arrays that a small relative to the number that they encode and rather
  imprecise. However, sparse arrays that are large relative to the number that they encode
  can be much more precise."
  [^long size ^long pos ^long range]
  (let [top (int (Math/floor (/ (* range pos) size)))
        bottom (int (Math/ceil (/ (* range (dec pos)) size)))
        mid (int (Math/floor (/ (+ top bottom) 2)))]
    [top mid bottom]))

(defn index-of-first-one
  [bit-seq]
  (first (keep-indexed #(when (= 1 %2) %1) bit-seq)))

(defn set-bit-pos
  [bit-seq]
  (- (count bit-seq) (index-of-first-one bit-seq)))

(defn single-bit-in-seq->num
  "Turns a bit sequence with a single bit set back into a value within the specified range"
  [bit-seq ^long range]
  (second (bit-pos->num (count bit-seq) (set-bit-pos bit-seq) range)))
