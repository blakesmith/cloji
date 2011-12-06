(ns cloji.test.core
  (:use [cloji.core])
  (:use [clojure.test]))

(deftest byte-array-int-test
  (is (= 28005 (byte-array-int [109 101]))))
