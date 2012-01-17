(ns cloji.test.huff-cdic
  (:use
    [cloji.huff-cdic]
    [cloji.core]
    [cloji.decoder]
    [cloji.test.helper]
    [clojure.test]))

(def hf (mobi-fixture "huff.mobi"))

(def huff (decode-headers hf))

(def huff-record
  (read-record huff hf (:first-huff-rec (:mobi-header huff))))

(deftest huff-table-impl
  (testing "meta-info collection, first and last item"
    (is (= [14 0 262143] (first (:meta-info (huff-table huff-record)))))
    (is (= [5 128 4294967295] (last (:meta-info (huff-table huff-record))))))
  (testing "mincode collection"
    (is (= 0 (first (first (:limits (huff-table huff-record))))))
    (is (= 4026531840 (first (nth (:limits (huff-table huff-record)) 5))))
    (is (= 989855744 (first (nth (:limits (huff-table huff-record)) 9)))))
  (testing "maxcode collection"
    (is (= 4294967295 (last (first (:limits (huff-table huff-record))))))
    (is (= 4160749567 (last (nth (:limits (huff-table huff-record)) 6))))
    (is (= 661651455 (last (nth (:limits (huff-table huff-record)) 12))))))

(deftest cdic-table-impl
  (testing "entries in the cdic table"
    (is (= ["e ", 32768] (first (cdic-table huff hf "UTF-8"))))
    (is (= ["q", 32768] (last (cdic-table huff hf "UTF-8")))))
  (testing "cdic phrase count"
    (is (= 786 (count (cdic-table huff hf "UTF-8"))))))

