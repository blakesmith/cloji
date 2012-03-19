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

(defn- record-sizes [records]
  (map count records))

(defn- record-offsets [records offset]
  (reduce
   (fn [offsets r] (conj offsets (+ (last offsets) r)))
   [offset]
   (record-sizes records)))

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

(defn- populate-record-maps [headers records]
  (let [record-size (record-map-length (count records))
        header-size (header-length attributes/static-attributes)]
    (assoc headers :record-list (record-maps records (+ record-size header-size)))))

(defn- populate-total-record-count [headers count]
  (assoc headers :record-count count))

(defn- populate-body-record-count [headers count]
  (assoc-in headers [:palmdoc-header :record-count] count))

(defn- populate-seed-id [headers]
  (assoc headers :seed-id (rand-int 5000)))

(defn- populate-text-length [headers records]
  (assoc-in headers [:palmdoc-header :text-length] (reduce + (record-sizes records))))

(defn- fill-headers [headers records]
  (-> headers
      (populate-total-record-count (count records))
      (populate-body-record-count (count records))
      (populate-text-length records)
      (populate-record-maps records)
      (populate-seed-id)))

(defn encode-mobi [headers body charset]
  (let [records (encode-body body charset)
        h (encode-headers (fill-headers headers records))]
    (into [] [h (flatten records)])))
  
