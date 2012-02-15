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
  (vector 2 (bit-xor (int (nth text offset)) 0x80)))

(defn- pass-through [text offset]
  (vector 1 (nth text offset)))

;; (defmacro fall-through-chain [& forms]
;;   "Evaluate a list of (pred) (body) forms, return the result of the first body where
;; pred evaluates to true and body is not nil"
;;   `(first (filter
;;            #(not (nil? %))
;;            (map
;;             (fn [step#]
;;               (let [[test# exp#] step#]
;;                 (when (eval test#)
;;                   (eval exp#))))
;;             (partition 2 '~forms)))))

(defmacro fall-through-chain [& clauses]
  (when clauses
    (list 'if (first clauses)
          (if-let [exp# (and (next clauses) (second clauses))]
            exp#
            (throw (IllegalArgumentException. "Requires an even number of forms")))
          (cons 'fall-through-chain (next (next clauses))))))
            
                             
(defn- compression-chain [text us offset textlength]
  (fall-through-chain 
   (and (> offset 10) (> (- textlength offset) 10))
   (type-b-compress (take offset text) offset)
   (and (< (inc offset) textlength) (= \space (nth us (inc offset))))
   (type-c-compress us offset)
   (let [ch (nth text offset)] (or (= ch 0) (and (>= ch 9) (< ch 0x80))))
   (pass-through us offset)))

(defn compressed-palmdoc [s charset]
  {:pre [(<= (count s) 4096)]}
  (let [textlength (count s)]
    (loop [us s cs [] offset 0]
      (if-let [next-char (first us)]
        (let [[n chunk] (compression-chain s us offset textlength)]
          (recur (drop n us) (into cs chunk) (+ offset n)))
        cs))))
          
