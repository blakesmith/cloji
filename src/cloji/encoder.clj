(ns cloji.encoder
  (:use [cloji.core]
        [cloji.attributes]
        [clojure.contrib.seq-utils :only [find-first]])
  (:require [cloji.palmdoc :as palmdoc]))

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

(defn- type-c-compress [text offset]
  (vector 2 [(bit-xor (int (nth text offset)) 0x80)]))

(defn- pass-through [text offset]
  (vector 1 [(int (nth text offset))]))

(defn- compression-chain [text offset textlength]
  (when (and (> offset 10) (> (- textlength offset) 10))
    (when-let [comp (type-b-compress (take offset text) offset)]
      comp))
  (when (and (< (inc offset) textlength) (= \space (nth text (inc offset))))
    (when-let [comp (type-c-compress text offset)]
      comp))
  (let [ch (int (nth text offset))]
    (when (or (= ch 0) (and (>= ch 9) (< ch 0x80)))
      (when-let [comp (pass-through text offset)]
        comp))))

(defn compressed-palmdoc [s charset]
  {:pre [(<= (count s) 4096)]}
  (let [textlength (count s)]
    (loop [cs [] offset 0]
      (if-let [next-char (nth s offset nil)]
        (let [[n chunk] (compression-chain s offset textlength)]
          (recur (into cs chunk) (+ offset n)))
        cs))))
          
