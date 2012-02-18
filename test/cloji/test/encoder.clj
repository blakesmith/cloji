(ns cloji.test.encoder
  (:use [clojure.test])
  (:require [cloji.encoder :as encoder]))

(deftest compressed-palmdoc-impl
  (testing "exception raising with string sizes > 4096 bytes"
    (is (thrown? java.lang.AssertionError (encoder/compressed-palmdoc (apply str (take 4097 (repeat \a))) "UTF-8"))))
  (testing "pass through compression of simple ascii only strings"
    (is (= [121 111 121 111 121 111] (encoder/compressed-palmdoc "yoyoyo" "UTF-8"))))
  (testing "encoding UTF8 characters (type a compression)"
    (is (= [3 226 128 152] (encoder/compressed-palmdoc "\u2018" "UTF-8"))))
  (testing "type c encoding, character followed by a space"
    (is (= [244] (encoder/compressed-palmdoc " t" "UTF-8")))
    (is (= [32 3 226 128 152] (encoder/compressed-palmdoc " \u2018" "UTF-8")))))
    

