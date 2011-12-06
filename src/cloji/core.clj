(ns cloji.core
  (:import [java.io RandomAccessFile]))

(defn read-bytes [input-stream n]
  (loop [coll [] bytes-read 0]
    (let [next-byte (.read input-stream)]
      (if (or (= next-byte -1) (= bytes-read n))
        coll
        (recur (conj coll next-byte) (inc bytes-read))))))

