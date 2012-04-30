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

(def none-type
  {:decode (fn [coll] coll)
   :encode (fn [v _] v)})

(def boolean-type
  {:decode (fn [coll] (= 1 ((:decode byte-array-int) coll)))
   :encode (fn [v len] ((:encode byte-array-int) (if v 1 0) len))})

(def exth-type-mappings
  {100 {:name :author :type mobi-string}
   101 {:name :publisher :type mobi-string}
   103 {:name :description :type mobi-string}
   104 {:name :isbn :type mobi-string}
   105 {:name :subject :type mobi-string}
   106 {:name :publish-date :type mobi-string}
   109 {:name :rights :type mobi-string}
   112 {:name :source :type mobi-string}
   115 {:name :sample :type boolean-type}
   116 {:name :start-reading :type byte-array-int}
   118 {:name :retail-price :type mobi-string}
   119 {:name :currency :type mobi-string}
   201 {:name :cover-offset :type byte-array-int}
   202 {:name :thumb-offset :type byte-array-int}
   203 {:name :fake-cover :type boolean-type}
   204 {:name :creator :type byte-array-int}
   205 {:name :creator-major :type byte-array-int}
   206 {:name :creator-minor :type byte-array-int}
   207 {:name :creator-build :type byte-array-int}
   300 {:name :font-signature :type none-type}})

(def encoding-type
  (let [types {65001 :utf-8
               1252 :cp1252}]
    {:decode (fn [coll] ((:decode type-lookup) types coll))
     :encode (fn [v len] ((:encode type-lookup) types v len))}))


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

(def exth-type
  (let [types (into {} (for [[k v] exth-type-mappings] [k (:name v)]))]
    {:decode (fn [coll] ((:decode type-lookup) types coll))}))

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

(defn pad-to-multiple-of-four [s]
  (let [size (count s)]
    (into s (take
             (if (zero? (rem size 4))
               0
               (- 4 (rem size 4)))
             (repeat 0)))))

(defn- encode-full-name [v len]
  (-> ((:encode mobi-string) v (count v))
      (conj 0 0)
      (pad-to-multiple-of-four)))

(defn- decode-full-name [coll]
  ((:decode mobi-string) coll))

(def full-name
  {:encode encode-full-name
   :decode decode-full-name})

(def exth-flags
  {:decode (fn [coll] (= 1 (bit-and (bit-shift-right ((:decode byte-array-int) coll) 6) 1)))
   :encode (fn [v len] ((:encode byte-array-int) (bit-shift-left (if v 1 0) 6) len))})

(def pdb-attributes
  [{:field :name :type mobi-string :len 32 :default "My eBook"}
   {:field :attributes :type palmdoc-attributes :len 2 :default []}
   {:field :version :type byte-array-int :len 2 :default 0}
   {:field :creation-date :type mobi-date :len 4 :default (new java.util.Date)}
   {:field :modification-date :type mobi-date :len 4 :default (new java.util.Date)}
   {:field :backup-date :type mobi-date :len 4 :default nil}
   {:field :modification-number :type byte-array-int :len 4 :default 0}
   {:field :appinfo-offset :type byte-array-int :len 4 :default 0}
   {:field :sortinfo-offset :type byte-array-int :len 4 :default 0}
   {:field :type :type mobi-string :len 4 :default "BOOK"}
   {:field :creator :type mobi-string :len 4 :default "MOBI"}
   {:field :seed-id :type byte-array-int :len 4}
   {:field :next-record-id :type byte-array-int :len 4 :default 0}
   {:field :record-count :type byte-array-int :len 2}])

(def record-attributes
  [{:field :data-offset :type byte-array-int :len 4}
   {:field :attributes :type record-attrs :len 1}
   {:field :id :type byte-array-int :len 3}])

(def palmdoc-attributes
  [{:field :compression :type byte-array-int :len 2 :default 2}
   {:field :text-length :type byte-array-int :len 4 :skip 2}
   {:field :record-count :type byte-array-int :len 2}
   {:field :record-size :type byte-array-int :len 2 :default 4096}
   {:field :current-position :type byte-array-int :len 4 :default 0}])

(def mobi-attributes
  [{:field :identifier :type mobi-string :len 4 :default "MOBI"}
   {:field :header-length :type byte-array-int :len 4}
   {:field :mobi-type :type mobi-type :len 4 :default 2}
   {:field :encoding :type encoding-type :len 4 :default 65001}
   {:field :unique-id :type byte-array-int :len 4 :default (int (* (rand) 1000000000))}
   {:field :file-version :type byte-array-int :len 4 :default 6}
   {:field :ortographic-index :type byte-array-int :len 4 :default 0xFFFFFFFF}
   {:field :inflection-index :type byte-array-int :len 4 :default 0xFFFFFFFF}
   {:field :index-names :type byte-array-int :len 4 :default 0xFFFFFFFF}
   {:field :index-keys :type byte-array-int :len 4 :default 0xFFFFFFFF}
   {:field :extra-index-0 :type byte-array-int :len 4 :default 0xFFFFFFFF}
   {:field :extra-index-1 :type byte-array-int :len 4 :default 0xFFFFFFFF}
   {:field :extra-index-2 :type byte-array-int :len 4 :default 0xFFFFFFFF}
   {:field :extra-index-3 :type byte-array-int :len 4 :default 0xFFFFFFFF}
   {:field :extra-index-4 :type byte-array-int :len 4 :default 0xFFFFFFFF}
   {:field :extra-index-5 :type byte-array-int :len 4 :default 0xFFFFFFFF}
   {:field :first-nonbook-offset :type byte-array-int :len 4 :default 0}
   {:field :full-name-offset :type byte-array-int :len 4}
   {:field :full-name-length :type byte-array-int :len 4}
   {:field :locale :type byte-array-int :len 4 :default 9}
   {:field :input-language :type byte-array-int :len 4 :default 0}
   {:field :output-language :type byte-array-int :len 4 :default 0}
   {:field :min-version :type byte-array-int :len 4 :default 6}
   {:field :first-image-offset :type byte-array-int :len 4 :default 0}
   {:field :first-huff-rec :type byte-array-int :len 4 :default 0}
   {:field :huff-rec-count :type byte-array-int :len 4 :default 0}
   {:field :huff-table-offset :type byte-array-int :len 4 :default 0}
   {:field :huff-table-length :type byte-array-int :len 4 :default 0}
   {:field :exth-flags :type exth-flags :len 4 :default false}
   {:field :drm-offset :type byte-array-int :len 4 :default 0xFFFFFFFF :skip 32}
   {:field :drm-count :type byte-array-int :len 4 :default 0xFFFFFFFF}
   {:field :drm-size :type byte-array-int :len 4 :default 0}
   {:field :drm-flags :type byte-array-int :len 4 :default 0}])

(def exth-attributes
  [{:field :identifier :type mobi-string :len 4 :default "EXTH"}
   {:field :header-length :type byte-array-int :len 4}
   {:field :record-count :type byte-array-int :len 4}])

(def extra-flag-attributes
  [{:field :extra-flags :type byte-array-int :len 2 :default 0}])

(def full-name-attributes
  [{:field :full-name :type full-name :len nil :default "My eBook"}])

(def static-attributes
  (list
   pdb-attributes
   palmdoc-attributes
   mobi-attributes))

(defn header-length [attrs]
  (reduce
   (fn [sum a]
     (+ sum (:len a) (if (:skip a) (:skip a) 0)))
   0
   (flatten attrs)))

(defn record-map-length [n]
  (* n (header-length record-attributes)))

