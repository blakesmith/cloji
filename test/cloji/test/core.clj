(ns cloji.test.core
  (:use [cloji.core]
        [cloji.test.helper]
        [clojure.test]))

(deftest single-byte-array-int-test
  (testing "Single byte array"
    (is (= 64 ((:decode byte-array-int) [0 64]))))
  (testing "Multi byte array"
    (is (= 28005 ((:decode byte-array-int) [109 101])))))

(deftest bitfield-with-values
  (testing "Single value"
    (is (= [:read-only] ((:decode bitfield) 2 {:read-only 0x0002}))))
  (testing "Multi values"
    (is (= [:read-only :app-info-dirty] ((:decode bitfield) 6 {:read-only 0x0002 :app-info-dirty 0x0004})))))

(deftest bitset-impl
  (testing "with a small max size"
    (is (= [true true false false false] (bitset 3 5))))
  (testing "with the default max size"
    (is (= [true true false false false false false false false false false false false false false false] (bitset 3)))))

(deftest bvw-int-impl
  (testing "backwards variable width integer encoding"
    (is (= 0x11111 (bvw-int [0x84 0x22 0x11])))))
