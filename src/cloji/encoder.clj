(ns cloji.encoder
  (:use [cloji.core]
        [cloji.attributes]
        [clojure.contrib.seq-utils :only [find-first]])
  (:require [cloji.palmdoc :as palmdoc]))

(defmacro condf [& forms]
  `(or
    ~@(map
       (fn [f]
         `(and
           ~(first f)
           ~(second f)))
       (partition 2 forms)) nil))

(defn- char-bytes [s offset charset]
  (map #(bit-and 0xff %) (seq (.getBytes (subs s offset (inc offset)) charset))))

(defn- count-duplicate-bytes [bytes offset]
  (count (take-while #(= (nth bytes offset) %) bytes)))

(defn- type-a-compress [text offset charset]
  (let [cb (char-bytes text offset charset)
        out-bytes
        (reduce
         (fn [compressed b]
           (let [nb (count-duplicate-bytes cb offset)]
             (into compressed
                   (if (= 1 nb)
                     (vector b)
                     (vector nb b)))))
         []
         cb)]
    (vector (count cb) out-bytes)))

(defn- type-b-compress [text offset]
  (if-let [matched-data (find-first
                         (fn [match-chunk]
                           (let [[match chunk] match-chunk]
                             (and (>= match 0) (<= (- offset match) 2047))))
                         (map
                          (fn [i]
                            (let [chunk (subs text offset (+ i offset))]
                              (vec (.indexOf text chunk) (count chunk))))
                          (range 10 2 -1)))]
    (let [[distance chunk-size] matched-data
          m (- offset distance)]
      (when (and (<= chunk-size 10) (>= chunk-size 3))
        (vector chunk-size
                (packed-int
                 (+ 0x8000 (bit-and (bit-shift-left m 3) 0x3ff8) (- chunk-size 3))
                 2))))))

(defn- type-c-compress [text offset charset]
  "Type C Compression - an ascii character followed by a space. Multibyte characters
should return nil from this function"
  (let [cb (char-bytes text offset charset)]
    (when (= (count cb) 1)
      (vector 2 (map #(bit-xor % 0x80) cb)))))

(defn- pass-through [text offset charset]
  "Pass through, write the bytes straight to the compression stream"
  (vector 1 (char-bytes text offset charset)))

(defn- compression-chain [text offset textlength charset]
  (or
   (condf
    (and (> offset 10) (> (- textlength offset) 10))
    (type-b-compress (take offset text) offset)
    (and (< (inc offset) textlength) (= \space (nth text (inc offset))))
    (type-c-compress text offset charset)
    (let [ch (int (nth text offset))]
      (or (= ch 0) (and (>= ch 9) (< ch 0x80))))
    (pass-through text offset charset))
   (type-a-compress text offset charset)))

(defn compressed-palmdoc [s charset]
  {:pre [(<= (count s) 4096)]}
  (let [textlength (count s)]
    (loop [cs [] offset 0]
      (if (< textlength (inc offset))
        cs
        (let [[n chunk] (compression-chain s offset textlength charset)]
          (recur (into cs chunk) (+ offset n)))))))

