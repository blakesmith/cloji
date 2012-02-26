(ns cloji.test.palmdoc
  (:use [clojure.test]
        [cloji.test.helper])
  (:require [cloji.encoder :as encoder]
            [cloji.decoder :as decoder]
            [cloji.core :as core]
            [cloji.attributes :as attributes]
            [cloji.palmdoc :as palmdoc]))

(def ni (mobi-fixture "no_images.mobi"))
(def no-images (decoder/decode-headers ni))

(deftest pack-palmdoc-impl
  (testing "exception raising with string sizes > 4096 bytes"
    (is (thrown? java.lang.AssertionError (palmdoc/pack (apply str (take 4097 (repeat \a))) "UTF-8"))))
  (testing "pass through compression of simple ascii only strings"
    (is (= [121 111 121 111 121 111] (palmdoc/pack "yoyoyo" "UTF-8")))
    (is (= [0 0] (palmdoc/pack "\u0000\u0000" "UTF-8"))))
  (testing "encoding UTF8 characters (type a compression)"
    (is (= [3 226 128 152] (palmdoc/pack "\u2018" "UTF-8"))))
  (testing "type c encoding, character followed by a space"
    (is (= [244] (palmdoc/pack " t" "UTF-8")))
    (is (= [32 3 226 128 152] (palmdoc/pack " \u2018" "UTF-8")))
    (is (= [32 230] (palmdoc/pack "  f" "UTF-8"))))
  (testing "type b encoding"
    (let [test-string "<html><head><guide><reference title=\"CONTENTS\" type=\"toc\"aaaa"
          enc (palmdoc/pack test-string "UTF-8")
          expected (take 54 (core/read-record no-images ni 1))]
      (is (= test-string (attributes/palmdoc-string nil nil enc "UTF-8")))
      (is (= [112 128 128 116] (subvec enc 47 51)))))
  (testing "sliding window type b encoding"
    (let [sliding-window "<html><head><guide><reference title=\"CONTENTS\" type=\"toc\"  filepos=0000001117"
          enc (palmdoc/pack sliding-window "UTF-8")]
      (is (= sliding-window (attributes/palmdoc-string nil nil enc "UTF-8")))))
  (testing "record parity from encode and decode"
    (let [r1 (decoder/decode-record no-images ni 1)
          enc (palmdoc/pack r1 "UTF-8")
          r2 (attributes/palmdoc-string nil nil enc "UTF-8")]
      (is (= r1 r2))
      (is (>= 2332 (count enc))))))