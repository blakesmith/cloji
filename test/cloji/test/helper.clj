(ns cloji.test.helper
  (:import [java.io RandomAccessFile]))

(defn mobi-fixture [name]
  (RandomAccessFile. (str "fixtures/" name) "r"))

(def pbytes
  (let [f (mobi-fixture "no_images.mobi")]
    (.seek f 10532)
    (repeatedly 4096 #(.read f))))

