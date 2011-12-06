(ns cloji.core
  (:import [java.io RandomAccessFile]))

(defn as-string [coll]
  (apply str (map char (filter #(not (= 0 %)) coll))))

(defn byte-array-int [coll]
  (reduce +
    (map (fn [b i]
           (bit-shift-left b (* i 8)))
         coll (iterate dec (- (count coll) 1)))))

(defn bitfield [value mappings]
  (map first
    (filter #(< 0 (bit-and (last %) value)) mappings)))

(defn read-bytes [input-stream n]
  (loop [coll [] bytes-read 0]
    (let [next-byte (.read input-stream)]
      (if (or (= next-byte -1) (= bytes-read n))
        coll
        (recur (conj coll next-byte) (inc bytes-read))))))

