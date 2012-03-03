(ns cloji.attributes
  (:require [cloji.palmdoc :as palmdoc]
            [cloji.huff-cdic :as huff])
  (:use [cloji.core]))

(defn- bitfield-funs [fields]
  {:decode (fn [coll]
             ((:decode bitfield) ((:decode byte-array-int) coll) fields))
   :encode (fn [v len]
               ((:encode byte-array-int) ((:encode bitfield) v fields) len))})

(def encoding-string
  {:utf-8 "UTF-8"
   :cp1252 "CP1252"})

(def palmdoc-string
  {:decode  (fn [_ _ coll encoding]
              ((:decode mobi-string) (palmdoc/unpack coll) encoding))
   :encode (fn [coll encoding])})

(def huffman-string
  {:decode (fn [headers is coll encoding]
             (let [table (huff/huff-table (read-record headers is (:first-huff-rec (:mobi-header headers))))
                   cdic (huff/clean-cdic table (huff/cdic-table headers is encoding) encoding)]
               (huff/unpack coll table cdic)))
   :encode (fn [coll encoding])})

(def compression-fn
  {1 (fn [coll encoding] (mobi-string coll encoding))
   2 palmdoc-string
   17480 huffman-string})

(def type-lookup
  {:decode (fn [types coll]
             (get types ((:decode byte-array-int) coll)))
   :encode (fn [types v len]
             (let [lookup (first (filter #(= v (second %)) types))]
               ((:encode byte-array-int) (or (first lookup) 0) len)))})

(def encoding-type
  {:decode (fn [coll]
             ((:decode type-lookup)
              {65001 :utf-8
               1252 :cp1252} coll))
   :encode (fn [type])})

(def mobi-type
  (let [types {2 :mobi-book
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
               518 :html}]
  {:decode (fn [coll] ((:decode type-lookup) types coll))
   :encode (fn [v len] ((:encode type-lookup) types v len))}))

(def palmdoc-attributes
  (bitfield-funs {:res-db 0x0001
                  :read-only 0x0002
                  :appinfo-dirty 0x0004
                  :backup 0x0008
                  :install-newer 0x0010
                  :reset 0x0020
                  :no-copy 0x0040}))

(def record-attrs
  (bitfield-funs {:secret 0x0010
                  :in-use 0x0020
                  :dirty 0x0040
                  :delete 0x0080}))

(def pdb-attributes
  [{:field :name :type mobi-string :len 32 :default "My eBook"}
   {:field :attributes :type palmdoc-attributes :len 2 :default []}
   {:field :version :type byte-array-int :len 2 :default 0}
   {:field :creation-date :type mobi-date :len 4 :default (new java.util.Date)}
   {:field :modification-date :type mobi-date :len 4 :default (new java.util.Date)}
   {:field :backup-date :type mobi-date :len 4 :default nil}
   {:field :modification-number :type byte-array-int :len 4}
   {:field :appinfo-offset :type byte-array-int :len 4}
   {:field :sortinfo-offset :type byte-array-int :len 4}
   {:field :type :type mobi-string :len 4}
   {:field :creator :type mobi-string :len 4}
   {:field :seed-id :type byte-array-int :len 4}
   {:field :next-record-id :type byte-array-int :len 4}
   {:field :record-count :type byte-array-int :len 2}])

(def record-attributes
  [{:field :data-offset :type byte-array-int :len 4}
   {:field :attributes :type record-attrs :len 1}
   {:field :id :type byte-array-int :len 3}])

(def palmdoc-attributes
  [{:field :compression :type byte-array-int :len 2}
   {:field :text-length :type byte-array-int :len 4 :skip 2}
   {:field :record-count :type byte-array-int :len 2}
   {:field :record-size :type byte-array-int :len 2}
   {:field :current-position :type byte-array-int :len 4}])

(def mobi-attributes
  [{:field :header-length :type byte-array-int :len 4 :skip 4}
   {:field :mobi-type :type mobi-type :len 4}
   {:field :encoding :type encoding-type :len 4}
   {:field :first-image-offset :type byte-array-int :len 4 :skip 76}
   {:field :first-huff-rec :type byte-array-int :len 4}
   {:field :huff-rec-count :type byte-array-int :len 4}
   {:field :huff-table-offset :type byte-array-int :len 4}
   {:field :huff-table-length :type byte-array-int :len 4}])

(def extra-flag-attributes
  [{:field :extra-flags :type byte-array-int :len 2}])
