(ns cloji.decoder
  (:use
    [cloji.core]
    [cloji.attributes])
  (:require
    [cloji.huff-cdic :as huff]
    [cloji.palmdoc :as palmdoc])
  (:import
    [javax.imageio ImageIO]))

(def encoding-string
  {:utf-8 "UTF-8"
   :cp1252 "CP1252"})

(defn palmdoc-string [_ _ coll encoding]
  (as-string (palmdoc/unpack coll) encoding))

(defn huffman-string [headers is coll encoding])
;  (let [table huff/huff-table
;  (as-string (huff/unpack coll table cdic) encoding))

(def compression-fn
  {1 (fn [coll encoding] (as-string coll encoding))
   2 palmdoc-string
   17480 huffman-string})

(defn decode-attributes [attrs is]
  (into {}
    (for [[attr-name f len skip] attrs]
      [attr-name (f (read-bytes is len skip))])))

(defn decode-trail-size [flags data]
  "Detects the trailing entry size for a record"
  (loop [f flags size 0]
    (let [flag (last f)]
      (if (= (count f) 1)
        (if flag (+ (bit-and (last (drop-last size data)) 0x3) size) size)
        (recur (drop-last f) (if flag
                          (+ size (bvw-int (drop-last size data))) size))))))

(defn decode-record-info [attrs x is]
  (doall (map (fn [_]
    (decode-attributes attrs is)) (range x))))

(defn read-attributes [attrs is offset]
  (with-location offset is
    (decode-attributes attrs is)))

(defn decode-record [headers is n]
  "Decodes a text record"
  (let [encoding (encoding-string (:encoding (:mobi-header headers)))
        f (get compression-fn (:compression (:palmdoc-header headers)))
        data (read-record headers is n)
        trail-size (decode-trail-size (bitset (:extra-flags (:mobi-header headers))) data)]
    (f headers is (drop-last trail-size data) encoding)))

(defn decode-headers [is]
  "Takes a mobipocket RandomAccessFile and decodes the mobipocket headers, returns a map of header attributes that are necessary for decoding the body and extracting images"
  (with-location 0 is
    (let [pdb-header (decode-attributes pdb-attributes is)
          record-list (decode-record-info record-attributes (:record-count pdb-header) is)
          first-offset (:data-offset (first record-list))
          palmdoc-header
            (read-attributes palmdoc-attributes is first-offset)
          mobi-header (decode-attributes mobi-attributes is)
          extra-flags
            (if (or (= 0xE4 (:header-length mobi-header))
                    (= 0xE8 (:header-length mobi-header)))
              (read-attributes extra-flag-attributes is (+ first-offset 0xF2))
              0)]
      (-> pdb-header
        (assoc :record-list record-list)
        (assoc :palmdoc-header palmdoc-header)
        (assoc :mobi-header (conj extra-flags mobi-header))))))

(defn decode-image [headers is n]
  "Returns a BufferedImage from the mobi image record at offset n"
  (let [ri (record-info headers (+ n (:first-image-offset (:mobi-header headers))))
        b (byte-array (:read-size ri))]
    (with-location (:seek ri) is
      (do
        (.read is b 0 (:read-size ri))
        (ImageIO/read (clojure.java.io/input-stream b))))))

(defn decode-body [is]
  "Top level function to decode all text records and concatenate them together"
  (with-location 0 is
    (let [headers (decode-headers is)]
      (reduce str
        (map #(decode-record headers is %)
             (range 1 (inc (:record-count (:palmdoc-header headers)))))))))

