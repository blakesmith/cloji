(ns cloji.test.encoder
  (:use [clojure.test]
        [cloji.test.helper])
  (:require [cloji.encoder :as encoder]
            [cloji.decoder :as decoder]
            [cloji.core :as core]))

(def ni (mobi-fixture "no_images.mobi"))
(def no-images (decoder/decode-headers ni))

(deftest encode-headers-impl
  (testing "encoding the pdb document name"
    (is (= (subvec (encoder/encode-headers no-images) 0 31)[54 68 65 0x5F 41 64 76 65 0x6E 74 75 72 0x2D 68 65 72 0x6C 0x6F 63 0x6B 0x5F 48 0x6F 0x6C 0x6D 65 73 00 00 00 00]))))