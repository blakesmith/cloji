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
    (is (= [] (:attributes no-images))))
  (testing "version"
    (is (= 0 (:version no-images))))
  (testing "creation date"
    (is (= 1303171212000 (.getTime (:creation-date no-images)))))
  (testing "modification date"
    (is (= 1303171212000 (.getTime (:modification-date no-images)))))
  (testing "backup date"
    (is (= nil (:backup-date no-images)))))
