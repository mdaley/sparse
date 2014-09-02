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
    (is (thrown? AssertionError (num->single-bit-in-seq 10 9.01 9)))))

(deftest num-to-bit-seq-1
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

(deftest num-to-bit-seq-2
  (testing "numbers in a larger range set correct bit in bit sequence"
    (is (= (bit-seq-with-one-set-bit 31 1) (num->single-bit-in-seq 31 0 4367)))
    (is (= (bit-seq-with-one-set-bit 31 31) (num->single-bit-in-seq 31 4367 4367)))
    (is (= (bit-seq-with-one-set-bit 31 2) (num->single-bit-in-seq 31 120 4367)))
    (is (= (bit-seq-with-one-set-bit 31 31) (num->single-bit-in-seq 31 4367 4367)))
    (is (= (bit-seq-with-one-set-bit 31 31) (num->single-bit-in-seq 31 4367 4367)))
    (is (= (bit-seq-with-one-set-bit 31 31) (num->single-bit-in-seq 31 4367 4367)))))
