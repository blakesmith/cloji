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
        (vec chunk-size
             (+ 0x8000 (bit-and (bit-shift-left m 3) 0x3ff8) (- chunk-size 3)))))))

; Still needs proper string packing ^

(defn- type-c-compress [text offset]
  (bit-xor (int (nth text offset)) 0x80))

(defn compressed-palmdoc [s]
  {:pre [(<= (count s) 4096)]}
  (let [textlength (count str)]
    (loop [us s cs (str) offset 0]
      (if-let [ch (first us)]
        (cond
         (and (> offset 10) (> (- textlength offset) 10))
         (let [[n chunk] (type-b-compress (take offset s) offset)]
           (recur (drop n us) (str cs chunk) (+ offset n)))
         (and (< (inc offset) textlength) (= \space (nth us (inc offset))))
         (recur (drop 2 us) (str cs (type-c-compress us offset)) (+ offset 2))
         (or (= ch 0) (and (>= ch 9) (< ch 0x80)))
         (recur (drop 1 us) (str cs ch) (inc offset)))
        cs))))
