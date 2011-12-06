(ns cloji.test.decoder
  (:import [java.io RandomAccessFile])
  (:use [cloji.decoder])
  (:use [clojure.test]))

(defn mobi-fixture [name]
  (RandomAccessFile. (str "fixtures/" name) "r"))

(def no-images (decode-mobi (mobi-fixture "no_images.mobi")))

(deftest decode-mobi-impl
  (testing "palmdoc header"
    (is (= "The_Adventur-herlock_Holmes" (:name no-images))))
  (testing "attributes"
    (is (= [] (:attributes no-images)))))
