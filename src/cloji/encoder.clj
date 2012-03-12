(ns cloji.encoder
  (:require [cloji.attributes :as attributes]
            [cloji.palmdoc :as palmdoc])
  (:use [cloji.core]
        [cloji.attributes]))

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

(defn- record-maps [records offset])

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
        record-size (* 8 (count records))]))
