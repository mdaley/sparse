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
