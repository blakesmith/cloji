(ns cloji.palmdoc
  (:use [cloji.core]
        [cloji.attributes]
        [clojure.contrib.seq-utils :only [find-first]]))

(defmacro condf [& clauses]
  "Takes a set of test/expr pairs. If both expressions in
a pair don't evaluate to false or nil, return the result of the
second expression. If either fail, continue down the chain until
one does. Otherwise return the :else expression or nil"
  {:pre [(even? (count clauses))]}
  `(or
    ~@(map
       (fn [f]
         (let [[guard exp] f]
           (if (= :else guard)
             exp
             `(and ~guard ~exp))))
       (partition 2 clauses)) nil))

(defn- vec-slice [coll start end]
  (if (< (count coll) end)
    (subvec coll start)
    (subvec coll start end)))

(defn- index-of [coll val]
  (let [s (count val)
        vs (count coll)]
    (loop [pos 0]
      (if (< vs (+ pos s))
        -1
        (let [search (subvec coll pos (+ pos s))]
          (if (= search val)
            pos
            (recur (inc pos))))))))

(defn- char-bytes [s charset]
  (vec (map #(bit-and 0xff %) (.getBytes s charset))))

(defn- type-a-compress [byte-sequence offset]
  (let [cb (take-while
            #(or
              (and (<= 0x01 %) (>= 0x08 %))
              (and (<= 0x80 %) (>= 0xff %)))
            (vec-slice byte-sequence offset (+ offset 8)))]
        (vector (count cb) (into [(count cb)] cb))))

(defn- type-b-compress [byte-sequence preamble offset]
  (if-let [matched-data (find-first
                         (fn [match-chunk]
                           (let [[match chunk] match-chunk]
                             (and (>= match 0) (<= (- offset match) 2047))))
                         (map
                          (fn [i]
                            (let [chunk (vec-slice byte-sequence offset (+ i offset))]
                              (vector (index-of preamble chunk) (count chunk))))
                          (range 10 2 -1)))]
    (let [[distance chunk-size] matched-data
          m (- offset distance)]
      (when (and (<= chunk-size 10) (>= chunk-size 3))
        (when (= offset 2057) (prn matched-data))
        (vector chunk-size
                (packed-int
                 (+ 0x8000 (bit-and (bit-shift-left m 3) 0x3ff8) (- chunk-size 3))
                 2))))))

(defn- type-c-compress [byte-sequence offset]
  "Type C Compression - an ascii character followed by a space. Multibyte characters
should return nil from this function"
  (let [ch (nth byte-sequence offset)]
    (when (and (>= ch 0x40) (< ch 0x80))
      (vector 2 [(bit-xor ch 0x80)]))))

(defn- pass-through [byte-sequence offset]
  "Pass through, write the bytes straight to the compression stream"
  (vector 1 [(nth byte-sequence offset)]))

(defn- compression-chain [byte-sequence offset length]
   (condf
    (and (> offset 10) (> (- length offset) 10)) (type-b-compress byte-sequence (subvec byte-sequence 0 offset) offset)
    (and (< (inc offset) length) (= \space (char (nth byte-sequence offset)))) (type-c-compress byte-sequence (inc offset))
    (let [ch (nth byte-sequence offset)]
      (or (= ch 0) (and (>= ch 9) (< ch 0x80)))) (pass-through byte-sequence offset)
    :else (type-a-compress byte-sequence offset)))

(defn unpack
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
          (if-let [nnc (nth cs 1 nil)]
            (let [lz7 (bit-and 0x3FFF (bit-or (bit-shift-left nc 8) nnc))
                  distance (bit-shift-right lz7 3)
                  length (+ 3 (bit-and 7 lz7))
                  lz7-decode (fn []
                               (loop [coll us n 0]
                                 (if (= n length)
                                   coll
                                   (recur (conj coll (nth coll (- (count coll) distance))) (inc n)))))]
              (recur (drop 2 cs) (lz7-decode))))
        (and (<= 0xC0 nc) (>= 0xFF nc)) (recur (rest cs) (into us [32 (bit-xor nc 0x80)]))))))

(defn pack [s charset]
  {:pre [(<= (count s) 4096)]}
  (let [byte-sequence (char-bytes s charset) length (count byte-sequence)]
    (loop [cs [] offset 0]
      (if (< length (inc offset))
        cs
        (let [[n chunk] (compression-chain byte-sequence offset length)]
          (recur (into cs chunk) (+ offset n)))))))
