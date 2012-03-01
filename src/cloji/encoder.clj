(ns cloji.encoder
  (:require [cloji.attributes :as attributes])
  (:use [cloji.core]
        [cloji.attributes]))

(defn- encode-attributes [attrs values]
  (reduce into []
        (for [{:keys [field type len skip default]} attrs]
          (if-let [v (get values field)]
            ((:encode type) v len)
            ((:encode type) default len)))))

(defn- encode-record-info [record-meta]
  (reduce into []
          (map (fn [r] (encode-attributes attributes/record-attributes r))
               record-meta)))

(defn encode-headers [values]
  (let [pdb-header (encode-attributes pdb-attributes values)
        record-list (encode-record-info (:record-list values))]
    (into pdb-header record-list)))

(defn encode-record [headers s n])

