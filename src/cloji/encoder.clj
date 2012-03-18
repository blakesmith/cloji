(ns cloji.encoder
  (:require [cloji.attributes :as attributes]
            [cloji.palmdoc :as palmdoc])
  (:use [cloji.core]
        [cloji.attributes]))

(defn- header-length [attrs]
  (reduce
   (fn [sum a]
     (+ sum (:len a)))
   0
   (flatten attrs)))

(defn- record-map-length [n]
  (* n (header-length attributes/record-attributes)))

(defn- record-offsets [records offset]
  (reduce
   (fn [offsets r] (conj offsets (+ (last offsets) r)))
   [offset]
   (map count records)))

(defn- record-maps [records offset]
  (map
   (fn [id offset]
     {:data-offset offset :attributes '() :id id})
   (map #(* 2 %) (range (count records)))
   (record-offsets records offset)))

(defn- encode-attributes [attrs values]
  (reduce into []
          (for [{:keys [field type len skip default]} attrs]
            (let [encoded
                  (if-let [v (get values field)]
                    ((:encode type) v len)
                    ((:encode type) default len))]
              (if skip (into (vec (take skip (repeat 0))) encoded) encoded)))))

(defn- encode-record-info [record-meta]
  (reduce into []
          (map (fn [r] (encode-attributes attributes/record-attributes r))
               record-meta)))

(defn encode-headers [values]
  (let [pdb-header (encode-attributes attributes/pdb-attributes values)
        record-list (encode-record-info (:record-list values))
        two-byte-sep [0 0]
        palmdoc-header (encode-attributes attributes/palmdoc-attributes (:palmdoc-header values))
        mobi-header (encode-attributes attributes/mobi-attributes (:mobi-header values))]
    (reduce into pdb-header
            [record-list two-byte-sep palmdoc-header mobi-header])))

(defn encode-body [body charset]
  (let [size (count body)]
    (map (fn [start end]
           (palmdoc/pack
            (if (> end size)
              (subs body start)
              (subs body start end)) charset))
           (range 0 size 4096)
           (range 4096 (+ size 4096) 4096))))

(defn encode-mobi [headers body charset]
  (let [records (encode-body body charset)
        record-size (record-map-length (count records))]))
