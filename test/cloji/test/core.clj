(ns cloji.test.core
  (:use [cloji.core])
  (:use [clojure.test]))

(deftest single-byte-array-int-test
  (is (= 64 (byte-array-int [0 64]))))

(deftest multi-byte-array-int-test
  (is (= 28005 (byte-array-int [109 101]))))

