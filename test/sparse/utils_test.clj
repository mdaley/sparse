(ns sparse.utils-test
  (:require [clojure.test :refer :all]
            [sparse.utils :refer :all]))

  (defn bit-seq-with-one-set-bit
  [len n]
  (assert (<= n len))
  (assert (> n 0))
  (flatten (conj (repeat (- n 1) 0) '(1) (repeat (- len n) 0))))

(deftest num-to-bit-seq-parameter-validations
  (testing "number can't be above allowed range"
    (is (thrown? AssertionError (num->single-bit-in-seq 10 10 9))))
  (testing "range must be greater than zero"
    (is (thrown? AssertionError (num->single-bit-in-seq 10 0 0)))))

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

(deftest bits-to-binary-string
  (testing "Set of active bit positions turned into binary string correctly
            (don't forget that positions are zero-based)."
    (is (= "1" (bits->binarystring 1 #{0})))
    (is (= "0" (bits->binarystring 1 nil)))
    (is (= "0" (bits->binarystring 1 #{})))
    (is (= "00000" (bits->binarystring 5 nil)))
    (is (= "00000" (bits->binarystring 5 #{})))
    (is (= "1010101" (bits->binarystring 7 #{0 2 4 6})))
    (is (= "100000000000000000000" (bits->binarystring 21 #{20})))
    (is (= "111111111111111111011" (bits->binarystring 21 #{0 1 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20})))))

(deftest num-to-bit-seq-one-bit-limit
  (testing "numbers in a range always set bit 1 in a 1 bit size array. Silly test, sensible limit"
    (is (= '(1) (num->single-bit-in-seq 1 2345 2345)))
    (is (= '(1) (num->single-bit-in-seq 1 0 2345456)))
    (is (= '(1) (num->single-bit-in-seq 1 1 1)))
    (is (= '(1) (num->single-bit-in-seq 1 0 1)))
    (is (= '(1) (num->single-bit-in-seq 1 2345 2345)))
    (is (= '(1) (num->single-bit-in-seq 1 99998 99999)))))

(deftest num-to-bit-string-one-bit-only
  (testing "single bit value turned into length one bit string - smallest limit"
    (is (= "1" (bits->binarystring 1 )))))

(deftest num-to-bit-seq-range-same-as-number-of-bits
  (testing "numbers in range 0 to 9 into bit sequence of length 10"
    (is (= '(0 0 0 0 0 0 0 0 0 1) (num->single-bit-in-seq 10 0 9)))
    (is (= '(0 0 0 0 0 0 0 0 1 0) (num->single-bit-in-seq 10 1 9)))
    (is (= '(0 0 0 0 0 0 0 1 0 0) (num->single-bit-in-seq 10 2 9)))
    (is (= '(0 0 0 0 0 0 1 0 0 0) (num->single-bit-in-seq 10 3 9)))
    (is (= '(0 0 0 0 0 1 0 0 0 0) (num->single-bit-in-seq 10 4 9)))
    (is (= '(0 0 0 0 1 0 0 0 0 0) (num->single-bit-in-seq 10 5 9)))
    (is (= '(0 0 0 1 0 0 0 0 0 0) (num->single-bit-in-seq 10 6 9)))
    (is (= '(0 0 1 0 0 0 0 0 0 0) (num->single-bit-in-seq 10 7 9)))
    (is (= '(0 1 0 0 0 0 0 0 0 0) (num->single-bit-in-seq 10 8 9)))
    (is (= '(1 0 0 0 0 0 0 0 0 0) (num->single-bit-in-seq 10 9 9)))))

(deftest num-to-bit-seq-larger-range
  (testing "numbers in a larger range set correct bit in bit sequence"
    (is (= (bit-seq-with-one-set-bit 31 1) (num->single-bit-in-seq 31 0 4367)))
    (is (= (bit-seq-with-one-set-bit 31 31) (num->single-bit-in-seq 31 4367 4367)))
    (is (= (bit-seq-with-one-set-bit 31 1) (num->single-bit-in-seq 31 140 4367)))
    (is (= (bit-seq-with-one-set-bit 31 2) (num->single-bit-in-seq 31 141 4367)))
    (is (= (bit-seq-with-one-set-bit 31 31) (num->single-bit-in-seq 31 4367 4367)))
    (is (= (bit-seq-with-one-set-bit 31 31) (num->single-bit-in-seq 31 4366 4367)))
    (is (= (bit-seq-with-one-set-bit 31 30) (num->single-bit-in-seq 31 4226 4367)))))

(deftest num-to-bit-seq-number-smaller-than-number-of-bits
  (testing "numbers in a range small than the number of bits set the correct bit"
    (is (= (bit-seq-with-one-set-bit 5 1) (num->single-bit-in-seq 5 0 2)))
    (is (= (bit-seq-with-one-set-bit 5 3) (num->single-bit-in-seq 5 1 2)))
    (is (= (bit-seq-with-one-set-bit 5 5) (num->single-bit-in-seq 5 2 2)))
    (is (= (bit-seq-with-one-set-bit 129 1) (num->single-bit-in-seq 129 0 2)))
    (is (= (bit-seq-with-one-set-bit 129 65) (num->single-bit-in-seq 129 1 2)))
    (is (= (bit-seq-with-one-set-bit 129 129) (num->single-bit-in-seq 129 2 2)))
    (is (= (bit-seq-with-one-set-bit 2000 1) (num->single-bit-in-seq 2000 0 127)))
    (is (= (bit-seq-with-one-set-bit 2000 2000) (num->single-bit-in-seq 2000 127 127)))
    (is (= (bit-seq-with-one-set-bit 2000 1008) (num->single-bit-in-seq 2000 64 127)))))
