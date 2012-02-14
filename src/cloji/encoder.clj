(ns cloji.encoder
  (:use [cloji.core]
        [cloji.attributes])
  (:require [cloji.palmdoc :as palmdoc]))

(defn- type-b-compress [text offset])

(defn- type-c-compress [text offset]
  (bit-xor (int (nth text offset)) 0x80))

(defn compressed-palmdoc [str]
  {:pre [(<= (count str) 4096)]}
  (let [textlength (count str)]
    (loop [us str cs (str) offset 0]
      (cond
       (and (> offset 10) (> (- textlength offset) 10))
       (let [[n chunk] (type-b-compress us offset)]
         (recur (drop n us) (str cs chunk) (+ offset n)))
       (and (< (inc offset) textlength) (= \space (nth us (inc offset))))
       (recur (drop 2 us) (str cs (type-c-compress us offset) (+ offset 2))
       (let [ch (int (nth us offset))] (or (= ch 0) (and (>= ch 9) (< ch 0x80))))
       (recur (drop 1 us) (str cs (int (nth us offset))) (inc offset))))))
