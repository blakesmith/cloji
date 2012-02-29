(ns cloji.test.encoder
  (:use [clojure.test]
        [cloji.test.helper])
  (:require [cloji.encoder :as encoder]
            [cloji.decoder :as decoder]
            [cloji.core :as core]))

(def ni (mobi-fixture "no_images.mobi"))
(def no-images (decoder/decode-headers ni))

(deftest encode-headers-imp
  (let [headers (encoder/encode-headers no-images)]
    (testing "encoding the pdb document name"
      (is (= (subvec headers 0 31) [0x54 0x68 0x65 0x5F 0x41 0x64 0x76 0x65 0x6E 0x74 0x75 0x72 0x2D 0x68 0x65 0x72 0x6C 0x6F 0x63 0x6B 0x5F 0x48 0x6F 0x6C 0x6D 0x65 0x73 00 00 00 00])))
    (testing "encoding the palmdoc attributes"
      (is (= (subvec headers 31 33) [0 0])))
    (testing "encoding the version number"
      (is (= (subvec headers 33 35) [0 0])))))
    