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
    (is (= nil (:backup-date no-images))))
  (testing "modification number"
    (is (= 0 (:modification-number no-images))))
  (testing "appinfo offset"
    (is (= 0 (:appinfo-offset no-images))))
  (testing "sortinfo offset"
    (is (= 0 (:sortinfo-offset no-images))))
  (testing "type"
    (is (= "BOOK" (:type no-images))))
  (testing "creator"
    (is (= "MOBI" (:creator no-images))))
  (testing "unique seed id"
    (is (= 379 (:seed-id no-images))))
  (testing "next record list id"
    (is (= 0 (:next-record-id no-images))))
  (testing "record count"
    (is (= 190 (:record-count no-images))))
  (testing "record-list"
    (let [first-record (first (:record-list no-images))
          second-record (nth (:record-list no-images) 1)]
      (is (= 1600 (:data-offset first-record)))
      (is (= [] (:attributes first-record)))
      (is (= 0 (:id first-record)))
      (is (= 10532 (:data-offset second-record)))
      (is (= [] (:attributes second-record)))
      (is (= 2 (:id second-record))))
    (is (= 190 (count (:record-list no-images)))))
  (testing "palmdoc header"
    (is (= 2 (:palmdoc-header no-images)))))

