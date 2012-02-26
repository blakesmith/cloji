(ns cloji.attributes
  (:require [cloji.palmdoc :as palmdoc]
            [cloji.huff-cdic :as huff])
  (:use [cloji.core]))

(def encoding-string
  {:utf-8 "UTF-8"
   :cp1252 "CP1252"})

(defn palmdoc-string [_ _ coll encoding]
  ((:decode as-string) (palmdoc/unpack coll) encoding))

(defn huffman-string [headers is coll encoding]
  (let [table (huff/huff-table (read-record headers is (:first-huff-rec (:mobi-header headers))))
        cdic (huff/clean-cdic table (huff/cdic-table headers is encoding) encoding)]
    (huff/unpack coll table cdic)))

(def compression-fn
  {1 (fn [coll encoding] (as-string coll encoding))
   2 palmdoc-string
   17480 huffman-string})

(defn type-lookup [types coll]
  (get types ((:decode byte-array-int) coll)))

(def encoding-type
  {:decode (fn [coll]
             (type-lookup
              {65001 :utf-8
               1252 :cp1252} coll))
   :encode (fn [type])})

(def mobi-type
  {:decode (fn [coll]
             (type-lookup
              {2 :mobi-book
               3 :palmdoc-book
               4 :audio
               257 :news
               258 :news-feed
               259 :news-magazine
               513 :pics
               514 :world
               515 :xls
               516 :ppt
               517 :text
               518 :html} coll))
   :encode (fn [type])})

(def palmdoc-attributes
  {:decode (fn [coll]
             ((:decode bitfield) ((:decode byte-array-int) coll)
                       {:res-db 0x0001
                        :read-only 0x0002
                        :appinfo-dirty 0x0004
                        :backup 0x0008
                        :install-newer 0x0010
                        :reset 0x0020
                        :no-copy 0x0040}))
   :encode (fn [attrs])})

(def record-attrs
  {:decode (fn [coll]
             ((:decode bitfield) ((:decode byte-array-int) coll)
                       {:secret 0x0010
                        :in-use 0x0020
                        :dirty 0x0040
                        :delete 0x0080}))
   :encode (fn [attrs])})

(def pdb-attributes
  [[:name as-string 32]
   [:attributes palmdoc-attributes 2]
   [:version byte-array-int 2]
   [:creation-date as-date 4]
   [:modification-date as-date 4]
   [:backup-date as-date 4]
   [:modification-number byte-array-int 4]
   [:appinfo-offset byte-array-int 4]
   [:sortinfo-offset byte-array-int 4]
   [:type as-string 4]
   [:creator as-string 4]
   [:seed-id byte-array-int 4]
   [:next-record-id byte-array-int 4]
   [:record-count byte-array-int 2]])

(def record-attributes
  [[:data-offset byte-array-int 4]
   [:attributes record-attrs 1]
   [:id byte-array-int 3]])

(def palmdoc-attributes
  [[:compression byte-array-int 2]
   [:text-length byte-array-int 4 2]
   [:record-count byte-array-int 2]
   [:record-size byte-array-int 2]
   [:current-position byte-array-int 4]])

(def mobi-attributes
  [[:header-length byte-array-int 4 4]
   [:mobi-type mobi-type 4]
   [:encoding encoding-type 4]
   [:first-image-offset byte-array-int 4 76]
   [:first-huff-rec byte-array-int 4]
   [:huff-rec-count byte-array-int 4]
   [:huff-table-offset byte-array-int 4]
   [:huff-table-length byte-array-int 4]])

(def extra-flag-attributes
  [[:extra-flags byte-array-int 2]])
