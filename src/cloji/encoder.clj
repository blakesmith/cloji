(ns cloji.encoder
  (:require [cloji.attributes :as attributes])
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

(defn encode-headers [values]
  (let [pdb-header (encode-attributes attributes/pdb-attributes values)
        record-list (encode-record-info (:record-list values))
        two-byte-sep [0 0]
        palmdoc-header (encode-attributes attributes/palmdoc-attributes (:palmdoc-header values))
        mobi-header (encode-attributes (subvec attributes/mobi-attributes 0 2) (:mobi-header values))]
    (reduce into pdb-header
            [record-list two-byte-sep palmdoc-header mobi-header])))

(defn encode-record [headers s n])

