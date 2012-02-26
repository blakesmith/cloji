(ns cloji.test.helper
  (:import [java.io RandomAccessFile]))

(defn mobi-fixture [name]
  (RandomAccessFile. (str "fixtures/" name) "r"))
