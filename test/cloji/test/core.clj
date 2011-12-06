(ns cloji.test.core
  (:use [cloji.core])
  (:use [clojure.test]))

(deftest single-byte-array-int-test
  (testing "Single byte array"
    (is (= 64 (byte-array-int [0 64]))))
  (testing "Multi byte array"
    (is (= 28005 (byte-array-int [109 101])))))

(deftest bitfield-with-values
  (testing "Single value"
    (is (= [:read-only] (bitfield 2 {:read-only 0x0002}))))
  (testing "Multi values"
    (is (= [:read-only :app-info-dirty] (bitfield 6 {:read-only 0x0002 :app-info-dirty 0x0004})))))

