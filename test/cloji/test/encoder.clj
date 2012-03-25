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
      (is (= (subvec headers 32 34) [0 0])))
    (testing "encoding the version number"
      (is (= (subvec headers 34 36) [0 0])))
    (testing "encoding the creation date, modification date, backup date"
      (is (= (subvec headers 36 40) [0x4D 0xAC 0xD0 0x8C]))
      (is (= (subvec headers 40 44) [0x4D 0xAC 0xD0 0x8C]))
      (is (= (subvec headers 44 48) [0 0 0 0])))
    (testing "encoding the modification number"
      (is (= (subvec headers 48 52) [0 0 0 0])))
    (testing "encoding the appinfo offset"
      (is (= (subvec headers 52 56) [0 0 0 0])))
    (testing "encoding the sortinfo offset"
      (is (= (subvec headers 56 60) [0 0 0 0])))
    (testing "encoding the type"
      (is (= (subvec headers 60 64) [0x42 0x4F 0x4F 0x4B])))
    (testing "encoding the creator"
      (is (= (subvec headers 64 68) [0x4D 0x4F 0x42 0x49])))
    (testing "encoding the seed-id"
      (is (= (subvec headers 68 72) [0x0 0x0 0x01 0x7B])))
    (testing "encoding the next-record id"
      (is (= (subvec headers 72 76) [0 0 0 0])))
    (testing "encoding the record count"
      (is (= (subvec headers 76 78) [0 0xBE])))
    (testing "encoding the record list attributes (metadata)"
      (is (= (subvec headers 78 82) [0 0 0x06 0x40]))
      (is (= (subvec headers 82 83) [0]))
      (is (= (subvec headers 83 86) [0 0 0])))
    (testing "palmdoc header compression"
      (is (= (subvec headers 1600 1602) [0 02])))
    (testing "palmdoc header text-length"
      (is (= (subvec headers 1604 1608) [0 0x0B 0x23 0xED])))
    (testing "palmdoc record count"
      (is (= (subvec headers 1608 1610) [0 0xB3])))
    (testing "palmdoc record size"
      (is (= (subvec headers 1610 1612) [0x10 0])))
    (testing "palmdoc current position"
      (is (= (subvec headers 1612 1616) [0 0 0 0])))
    (testing "mobi header header length"
      (is (= (subvec headers 1620 1624) [0 0 0 0xE8])))
    (testing "mobi header mobi type"
      (is (= (subvec headers 1624 1628) [0 0 0 0x02])))
    (testing "mobi header encoding"
      (is (= (subvec headers 1628 1632) [0 0 0xFD 0xE9])))
    (testing "mobi header first image offset"
      (is (= (subvec headers 1708 1712) [0 0 0 0xB8])))
    (testing "mobi header first huff rec"
      (is (= (subvec headers 1712 1716) [0 0 0 0])))
    (testing "mobi header huff rec count"
      (is (= (subvec headers 1716 1720) [0 0 0 0])))
    (testing "mobi header huff table offset"
      (is (= (subvec headers 1720 1724) [0 0 0 0])))
    (testing "mobi header table length"
      (is (= (subvec headers 1724 1728) [0 0 0 0])))
    (testing "exth flags"
      (is (= (subvec headers 1728 1732) [0 0 0 64])))
    (testing "32 unknown bytes (blank)"
      (is (= (subvec headers 1732 1764) (take 32 (repeat 0)))))
    (testing "drm offset"
      (is (= (subvec headers 1764 1768) [0xff 0xff 0xff 0xff])))))

