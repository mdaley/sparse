(ns sparse.utils-test
  (:require [clojure.test :refer :all]
            [sparse.utils :refer :all]))

(defn- bitstring-with-one-set-bit
  [len n]
  (reduce (fn [s x] (str s (if (= x n) "1" "0"))) "" (reverse (range 0 len))))

(deftest bit-to-set-tests
  (testing "Correct bit to set returned (zero based)."
    (is (= 0 (bit-to-set 1 0 1)))
    (is (= 0 (bit-to-set 1 1 1)))
    (is (= 0 (bit-to-set 1 0 Long/MAX_VALUE)))
    (is (= 0 (bit-to-set 1 Long/MAX_VALUE Long/MAX_VALUE)))
    (is (= 0 (bit-to-set 27 0 999999)))
    (is (= 26 (bit-to-set 27 999999 999999)))
    (is (= 6 (bit-to-set 7 137 137)))
    (is (= 3 (bit-to-set 7 68 137)))
    (is (= 0 (bit-to-set 7 0 137)))
    (is (= 0 (bit-to-set 7 19 137)))
    (is (= 1 (bit-to-set 7 20 137)))
    (is (= 6 (bit-to-set 7 118 137)))
    (is (= 5 (bit-to-set 7 117 137)))
    (is (= 2 (bit-to-set 7 58 137)))
    (is (= 0 (bit-to-set 100 0 12)))
    (is (= 99 (bit-to-set 100 12 12)))
    (is (= 41 (bit-to-set 100 5 12)))))

(deftest bitstring->bit-to-set-tests
  (testing "Bitstring can be turned into a single bit to be set in sparse binary"
    (is (= 0 (bitstring->bit-to-set 14 "0")))
    (is (= 13 (bitstring->bit-to-set 14 "1")))
    (is (= 0 (bitstring->bit-to-set 12 "00000000")))
    (is (= 3  (bitstring->bit-to-set 12 "0100001")))
    (is (= 11 (bitstring->bit-to-set 12 "11111")))
    (is (= 0 (bitstring->bit-to-set 100 "00000000000")))
    (is (= 57 (bitstring->bit-to-set 100 "1001001101101")))
    (is (= 99 (bitstring->bit-to-set 100 "1111111111")))))

(deftest bits->binarystring-tests
  (testing "bitstring can be created from length and a set of on-bit positions"
    (is (= "1" (bits->binarystring 1 #{0})))
    (is (= "0" (bits->binarystring 1 #{})))
    (is (= "1010101" (bits->binarystring 7 #{6 4 2 0})))
    (is (= "0000000100000100000011000001" (bits->binarystring 28 #{0 6 7 14 20})))))

(deftest long->bitstring-tests
  (testing "Create a bitstring from a long value"
    (is (= "0" (long->bitstring 0)))
    (is (= "11111" (long->bitstring 31)))
    (is (= "00000000" (long->bitstring 0 8)))
    (is (= "00011111" (long->bitstring 31 8)))))

(deftest long->bitranges-tests
  (testing "work out bit ranges for a number"
    (is (= '(4 3) (long->bit-ranges 127 2)))
    (is (= '(1 1 1 1 1 1 1) (long->bit-ranges 127 7)))
    (is (= '(1 1 1 1 1 1 1 1 1 1) (long->bit-ranges 16 10)))))

(deftest split-bitstring-tests
  (testing "make sure that bitstrings get split properly by specified ranges"
    (is (= '("1") (split-bitstring "1" '(1))))
    (is (= '("1111" "0000" "111" "0" "1") (split-bitstring "1111000011101" '(4 4 3 1 1))))))
