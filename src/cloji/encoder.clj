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

(defn- char-bytes [s length offset charset]
  (let [strlen (count s)
        requested-len (+ offset length)
        l (if (> requested-len strlen) strlen requested-len)]
    (map #(bit-and 0xff %) (seq (.getBytes (subs s offset l) charset)))))

(defn- count-duplicate-bytes [bytes offset]
  (dec (count (take-while #(= (nth bytes offset) %) bytes))))

(defn- type-a-compress [text offset charset]
  (let [cb (char-bytes text 8 offset charset)
        db (count-duplicate-bytes cb 0)]
    (prn cb)
    (if (= 0 db)
      (vector 1 [(first cb)])
      (vector db [db (first cb)]))))

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
  (let [cb (char-bytes text 1 offset charset)]
    (vector 2 [(bit-xor (first cb) 0x80)])))

(defn- pass-through [text offset charset]
  "Pass through, write the bytes straight to the compression stream"
  (vector 1 (char-bytes text 1 offset charset)))

(defn- compression-chain [text offset textlength charset]
   (condf
    (and (> offset 10) (> (- textlength offset) 10)) (type-b-compress (take offset text) offset)
    (and (< (inc offset) textlength) (= \space (nth text (inc offset)))) (type-c-compress text offset charset)
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

