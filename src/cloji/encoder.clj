(ns cloji.encoder
  (:use [cloji.core]
        [cloji.attributes])
  (:require [cloji.palmdoc :as palmdoc]))

(defn- type-b-compress [text offset]
  [1 1])

(defn- type-c-compress [text offset]
  (bit-xor (int (nth text offset)) 0x80))

(defn compressed-palmdoc [s]
  {:pre [(<= (count s) 4096)]}
  (let [textlength (count str)]
    (loop [us s cs (str) offset 0]
      (if-let [ch (first us)]
        (cond
         (and (> offset 10) (> (- textlength offset) 10))
         (let [[n chunk] (type-b-compress s offset)]
           (recur (drop 1 us) (str cs "1") (+ offset n)))
         (and (< (inc offset) textlength) (= \space (nth us (inc offset))))
         (recur (drop 2 us) (str cs (type-c-compress us offset)) (+ offset 2))
         (or (= ch 0) (and (>= ch 9) (< ch 0x80)))
         (recur (drop 1 us) (str cs ch) (inc offset)))
        cs))))
