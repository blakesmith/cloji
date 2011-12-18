(ns cloji.test.decoder
  (:use [cloji.decoder])
  (:use [cloji.test.helper])
  (:use [clojure.test]))

(def f (mobi-fixture "no_images.mobi"))
(def no-images (decode-headers f))

(deftest decode-headers-impl
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
    (is (= 2 (:compression (:palmdoc-header no-images))))
    (is (= 730093 (:text-length (:palmdoc-header no-images))))
    (is (= 179 (:record-count (:palmdoc-header no-images))))
    (is (= 4096 (:record-size (:palmdoc-header no-images))))
    (is (= 0 (:current-position (:palmdoc-header no-images)))))
  (testing "mobi header"
    (is (= 232 (:header-length (:mobi-header no-images))))
    (is (= :mobi-book (:mobi-type (:mobi-header no-images))))))

(deftest decode-record-impl
  (testing "decoding record n"
    (is (= "<html>" (apply str (take 6 (decode-record no-images f 1))))))
  (testing "decoding a record with out of bounds errors"
    (is (= "after" (apply str (take 5 (decode-record no-images f 8))))))
  (testing "decoding the last text record record"
    (is (= "old, " (apply str (take 5 (decode-record no-images f 178)))))))

