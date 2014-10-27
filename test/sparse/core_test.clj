(ns sparse.core-test
  (:require [clojure.test :refer :all]
            [sparse.core :refer :all]))

(deftest num->sparse-seq-validations
  (testing "Num to sparse seq rejects bad parameters"
    (is (thrown? AssertionError (long->sparse 12 3 0 0)))
    (is (thrown? AssertionError (long->sparse 0 0 1 1)))
    (is (thrown? AssertionError (long->sparse 12 13 1 1)))))

(deftest num->sparse-seq-calculations-are-correct
  (testing "Num to sparse seq calculations work correctly"
    (is  (= "000100010001" (long->sparse 12 3 0 1024)))
    (is  (= "0000100010001" (long->sparse 13 3 0 1024)))
    (is  (= "111" (long->sparse 3 3 0 1024)))
    (is  (= "00111" (long->sparse 5 3 0 1024)))
    (is  (= "0111" (long->sparse 4 3 1024 1024)))
    (is  (= "111111111111" (long->sparse 12 12 0 1024)))
    (is  (= "111111111111" (long->sparse 12 12 1024 1024)))
    (is  (= "00111111111111" (long->sparse 14 12 0 1024)))
    (is  (= "0000100001000010000100001" (long->sparse 25 5 0 1024)))
    (is  (= "00000000100010001000100010001001000010010" (long->sparse 41 9 999999 (Long/MAX_VALUE))))))
