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

(defn decomp-palmdoc
  "Takes a stream of bytes compressed using the Palmdoc compression algorithm
  and returns a sequence of decompressed bytes"
  [c]
  (loop [cs c us []]
    (let [nc (first cs)]
      (cond
        (= nil nc) us
        (= 0x00 nc) (recur (rest cs) (conj us nc))
        (and (<= 0x01 nc) (>= 0x08 nc)) (recur (drop (+ 1 nc) cs) (into us (take nc (rest cs))))
        (and (<= 0x09 nc) (>= 0x7F nc)) (recur (rest cs) (conj us nc))
        (and (<= 0x80 nc) (>= 0xBF nc))
          (try
            (let [lz7 (bit-and 0x3FFF (bit-or (bit-shift-left nc 8) (nth cs 1)))
                  distance (bit-shift-right lz7 3)
                  length (+ 3 (bit-and 7 lz7))]
              (recur (drop 2 cs) (into us (take length (drop (- (count us) distance) us)))))
            (catch java.lang.IndexOutOfBoundsException e us))
        (and (<= 0xC0 nc) (>= 0xFF nc)) (recur (rest cs) (into us [32 (bit-xor nc 0x80)]))))))

(defn palmdoc-string [coll]
  (apply str (map char (decomp-palmdoc coll))))

