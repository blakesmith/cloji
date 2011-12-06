(ns cloji.test.core
  (:use [cloji.core])
  (:use [clojure.test]))

(deftest single-byte-array-int-test
  (is (= 64 (byte-array-int [0 64]))))

(deftest multi-byte-array-int-test
  (is (= 28005 (byte-array-int [109 101]))))

(deftest bitfield-with-values
  (is (= [:read-only] (bitfield 2 {:read-only 0x0002}))))

(deftest bitfield-multi-values
  (is (= [:read-only :app-info-dirty] (bitfield 6 {:read-only 0x0002 :app-info-dirty 0x0004}))))

