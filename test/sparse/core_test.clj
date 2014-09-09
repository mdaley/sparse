(ns sparse.core-test
  (:require [clojure.test :refer :all]
            [sparse.core :refer :all]))

(defn bit-seq-with-one-set-bit
  [len n]
  (assert (<= n len))
  (assert (> n 0))
  (flatten (conj (repeat (- n 1) 0) '(1) (repeat (- len n) 0))))

(deftest num-to-bit-seq-parameter-validations
  (testing "number must be greater than zero"
    (is (thrown? AssertionError (num->single-bit-in-seq 10 -0.1 9))))
  (testing "number can't be above allowed range"
    (is (thrown? AssertionError (num->single-bit-in-seq 10 9.01 9))))
  (testing "range must be greater than zero"
    (is (thrown? AssertionError (num->single-bit-in-seq 10 0 0)))))

(deftest num-to-bit-seq-one-bit-limit
  (testing "numbers in a range always set bit 1 in a 1 bit size array. Silly test, sensible limit"
    (is (= '(1) (num->single-bit-in-seq 1 2345 2345)))
    (is (= '(1) (num->single-bit-in-seq 1 0 2345456)))
    (is (= '(1) (num->single-bit-in-seq 1 1 1)))
    (is (= '(1) (num->single-bit-in-seq 1 0 1)))
    (is (= '(1) (num->single-bit-in-seq 1 2345 2345)))
    (is (= '(1) (num->single-bit-in-seq 1 99998 99999)))))

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

(deftest base-power-calculations-validation
  (testing "Base power calculations fail with bad parameters"
    (is (thrown? AssertionError (num-as-base-power-multiples 123 1)))
    (is (thrown? AssertionError (num-as-base-power-multiples -000.1 10)))))

(deftest base-power-calculations-are-correct
  (testing "Base power calculations work correctly"
    (is (= '(0) (num-as-base-power-multiples 0 2)))
    (is (= '(1) (num-as-base-power-multiples 1 10)))
    (is (= '(1 2 3 4 5) (num-as-base-power-multiples 12345 10)))
    (is (= '(1 0) (num-as-base-power-multiples 2 2)))
    (is (= '(1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1) (num-as-base-power-multiples 174763 2)))
    (is (= '(1 0 0 0 2) (num-as-base-power-multiples 100 3.14159)))
    (is (= '(1) (num-as-base-power-multiples 1 3.14159)))
    (is (= '(1 0) (num-as-base-power-multiples 74.789 74.789)))
    (is (= '(2 5 4 0 11 14 3 15 15) (num-as-base-power-multiples 9999999999 16)))
    (is (= '(1) (num-as-base-power-multiples 1 1.0000001)))
    (is (= '(1 0) (num-as-base-power-multiples 1.0000001 1.0000001)))))
