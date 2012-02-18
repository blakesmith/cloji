(ns cloji.encoder
  (:use [cloji.core]
        [cloji.attributes]
        [clojure.contrib.seq-utils :only [find-first]])
  (:require [cloji.palmdoc :as palmdoc]))

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
  (map #(bit-and 0xff %) (seq (.getBytes s charset))))

(defn- count-duplicate-chars [chars]
  (count (take-while #(= (first chars) %) chars)))

(defn- get-subs [s length offset]
  (let [strlen (count s)
        requested-len (+ offset length)
        l (if (> requested-len strlen) strlen requested-len)]
    (subs s offset l)))

(defn- type-a-compress [text offset charset]
  (let [ss (get-subs text 1 offset)
        cb (char-bytes ss charset)]
    (vector 1 (into [(count cb)] cb))))

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
  (let [cb (char-bytes (get-subs text 1 offset) charset)]
    (when (= 1 (count cb))
      (vector 2 [(bit-xor (first cb) 0x80)]))))

(defn- pass-through [text offset charset]
  "Pass through, write the bytes straight to the compression stream"
  (vector 1 (char-bytes (get-subs text 1 offset) charset)))

(defn- compression-chain [text offset textlength charset]
   (condf
    (and (> offset 10) (> (- textlength offset) 10)) (type-b-compress (take offset text) offset)
    (and (< (inc offset) textlength) (= \space (nth text offset))) (type-c-compress text (inc offset) charset)
    (let [ch (int (nth text offset))]
      (or (= ch 0) (and (>= ch 9) (< ch 0x80)))) (pass-through text offset charset)
    :else (type-a-compress text offset charset)))

(defn compressed-palmdoc [s charset]
  {:pre [(<= (count s) 4096)]}
  (let [textlength (count s)]
    (loop [cs [] offset 0]
      (if (< textlength (inc offset))
        cs
        (let [[n chunk] (compression-chain s offset textlength charset)]
          (recur (into cs chunk) (+ offset n)))))))

