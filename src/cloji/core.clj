(ns cloji.core
  (:import [java.util Date]))

(defn byte-array-int [coll]
  (reduce +
    (map (fn [b i]
           (bit-shift-left b (* i 8)))
         coll (iterate dec (- (count coll) 1)))))

(defn bitfield [value mappings]
  (map first
    (filter #(< 0 (bit-and (last %) value)) mappings)))

(defn read-bytes [input-stream n skip]
  (when skip (.skipBytes input-stream skip))
  (loop [coll [] bytes-read 0]
    (if (= bytes-read n)
      coll
      (recur
        (let [next-byte (.read input-stream)]
          (if (= -1 next-byte)
            coll
            (conj coll next-byte))) (inc bytes-read)))))

(defn as-string [coll]
  (apply str (map char (filter #(not (= 0 %)) coll))))

(defn as-date [coll]
  (let [t (byte-array-int coll)]
    (when (not (= 0 t))
      (doto (new Date) (.setTime (* 1000 t))))))

