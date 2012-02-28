(ns cloji.encoder
  (:require [cloji.attributes :as attributes])
  (:use [cloji.core]
        [cloji.attributes]))

(defn- encode-attributes [attrs values]
  (prn attrs)
  (reduce into []
        (for [{:keys [field type len skip default]} attrs]
          (if-let [v (get values field)]
            ((:encode type) v len)
            ((:encode type) default len)))))

(defn encode-headers [values]
  (let [pdb-header (encode-attributes (subvec attributes/pdb-attributes 0 1) values)]
    pdb-header))

(defn encode-record [headers s n])

