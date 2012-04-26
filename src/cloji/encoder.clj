(ns cloji.encoder
  (:require [cloji.attributes :as attributes]
            [cloji.palmdoc :as palmdoc])
  (:import [java.io FileOutputStream ByteArrayOutputStream]
           [javax.imageio ImageIO])
  (:use [cloji.core]
        [cloji.attributes]))

(defn- record-sizes [records]
  (map count records))

(defn- full-name-length [s]
  (let [slen (+ 2 (count s))]
    (+ slen (count (take-while #(not= (rem % 4) 0) (iterate inc slen))))))

(defn- record-offsets
  [records encoded-images pdb-length offset]
  (reduce
   (fn [offsets r] (conj offsets (+ (last offsets) r)))
   [(+ 2 pdb-length) offset]
   (record-sizes (reduce into [records encoded-images]))))

(defn- record-maps
  [records encoded-images pdb-length total-offset]
  (map (fn [id offset] {:data-offset offset :attributes '() :id id})
   (map #(* 2 %) (range (+ (count records) (count encoded-images) 2)))
   (record-offsets records encoded-images pdb-length total-offset)))

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
        mobi-header (encode-attributes attributes/mobi-attributes (:mobi-header values))
        full-name (encode-attributes attributes/full-name-attributes values)]
    (reduce into pdb-header
            [record-list two-byte-sep palmdoc-header mobi-header full-name])))

(defn- encode-image [im]
  "Take a BufferedImage and output its raw bytes"
  (with-open [dos (ByteArrayOutputStream.)]
    (do
      (ImageIO/write im "jpg" dos)
      (.toByteArray dos))))
  
(defn encode-body [body charset]
  (let [size (count body)]
    (map (fn [start end]
           (palmdoc/pack
            (if (> end size)
              (subs body start)
              (subs body start end)) charset))
           (range 0 size 4096)
           (range 4096 (+ size 4096) 4096))))

(defn- populate-record-maps
  [headers records encoded-images pdb-length offset-to-body]
  (assoc headers :record-list (record-maps records encoded-images pdb-length (+ (full-name-length (:full-name headers)) offset-to-body))))

(defn- populate-total-record-count [headers count]
  (assoc headers :record-count (inc count)))

(defn- populate-body-record-count [headers count]
  (assoc-in headers [:palmdoc-header :record-count] count))

(defn- populate-seed-id [headers]
  (assoc headers :seed-id (rand-int 5000)))

(defn- populate-text-length [headers records]
  (assoc-in headers [:palmdoc-header :text-length] (reduce + (record-sizes records))))

(defn- populate-full-name-info [headers total-size]
  (-> headers
      (assoc-in [:mobi-header :full-name-length] (count (:full-name headers)))
      (assoc-in [:mobi-header :full-name-offset] total-size)))

(defn- populate-header-lengths [headers]
  (assoc-in headers [:mobi-header :header-length] (attributes/header-length attributes/mobi-attributes)))

(defn- populate-first-image-offset [headers idx]
  (assoc-in headers [:mobi-header :first-image-offset] idx))

(defn- populate-pdb-name [headers]
  (assoc headers :name (clojure.string/replace (:full-name headers) #"\s" "-")))

(defn- fill-headers [headers records encoded-images]
  (let [records-length (attributes/record-map-length (+ (count records) (count encoded-images) 2))
        pdb-length (+ records-length (attributes/header-length attributes/pdb-attributes))
        offset-to-body (+ 2 records-length (attributes/header-length attributes/static-attributes))
        full-name-offset (attributes/header-length (list attributes/palmdoc-attributes attributes/mobi-attributes))]
    (-> headers
        (populate-total-record-count (+ (count records) (count encoded-images) 1))
        (populate-body-record-count (count records))
        (populate-text-length records)
        (populate-full-name-info full-name-offset)
        (populate-record-maps records encoded-images pdb-length offset-to-body)
        (populate-first-image-offset (count records))
        (populate-seed-id)
        (populate-pdb-name)
        (populate-header-lengths))))

(defn encode-mobi [headers body charset encoded-images]
  (let [records (vec (encode-body body charset))
        h (encode-headers (fill-headers headers records encoded-images))]
    (into h (flatten records))))

(defn encode-to-file [headers body charset images file]
  (let [encoded-images (vec (map encode-image images))]
    (with-open [f (FileOutputStream. file)]
      (do
        (.write f (into-array Byte/TYPE (map #(.byteValue %) (encode-mobi headers body charset encoded-images))))
        (doseq [i encoded-images]
          (.write f i))))))
  
