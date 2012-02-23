(ns cloji.encoder
  (:use [cloji.core]
        [cloji.attributes]
        [clojure.contrib.seq-utils :only [find-first]])
  (:require [cloji.palmdoc :as palmdoc]))

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

(defn- char-bytes [s charset]
  (vec (map #(bit-and 0xff %) (.getBytes s charset))))

(defn- get-subs [s length offset]
  (let [strlen (count s)
        rend (+ offset length)
        roff (if (> rend strlen) strlen rend)]
    (subs s offset roff)))

(defn- type-a-compress [byte-sequence offset]
  (let [cb (take-while
            #(or
              (and (<= 0x01 %) (>= 0x08 %))
              (and (<= 0x80 %) (>= 0xff %)))
            (if (< (count byte-sequence) (+ offset 8))
              (subvec byte-sequence offset)
              (subvec byte-sequence offset (+ offset 8))))]
        (vector (count cb) (into [(count cb)] cb))))

(defn- type-b-compress [byte-sequence preamble offset]
  (if-let [matched-data (find-first
                         (fn [match-chunk]
                           (let [[match chunk] match-chunk]
                             (and (>= match 0) (<= (- offset match) 2047))))
                         (map
                          (fn [i]
                            (let [chunk (subvec byte-sequence offset (+ i offset))]
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

(defn- compression-chain [byte-sequence offset length charset]
   (condf
    (and (> offset 10) (> (- length offset) 10)) (type-b-compress byte-sequence (subvec byte-sequence 0 offset) offset)
    (and (< (inc offset) length) (= \space (char (nth byte-sequence offset)))) (type-c-compress byte-sequence (inc offset))
    (let [ch (nth byte-sequence offset)]
      (or (= ch 0) (and (>= ch 9) (< ch 0x80)))) (pass-through byte-sequence offset)
    :else (type-a-compress byte-sequence offset)))

(defn compressed-palmdoc [s charset]
  {:pre [(<= (count s) 4096)]}
  (let [byte-sequence (char-bytes s charset) length (count byte-sequence)]
    (loop [cs [] offset 0]
      (if (< length (inc offset))
        cs
        (let [[n chunk] (compression-chain byte-sequence offset length charset)]
          (recur (into cs chunk) (+ offset n)))))))

