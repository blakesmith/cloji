(ns cloji.decoder
  (:use [cloji.core]))

(defn decode-palmdoc-attributes [coll]
  (bitfield (byte-array-int coll)
            {:res-db 0x0001
             :read-only 0x0002
             :appinfo-dirty 0x0004
             :backup 0x0008
             :install-newer 0x0010
             :reset 0x0020
             :no-copy 0x0040}))

(defn decode-record-attributes [coll]
  (bitfield (byte-array-int coll)
            {:secret 0x0010
             :in-use 0x0020
             :dirty 0x0040
             :delete 0x0080}))

(defn decode-attributes [attrs input-stream]
  (into {}
    (for [[attr-name len f] attrs]
      [attr-name (f (read-bytes input-stream len))])))

(defn decode-record-info [attrs x input-stream]
  (doall (map (fn [_]
    (decode-attributes attrs input-stream)) (range x))))

(def pdb-attributes
  [[:name 32 as-string]
   [:attributes 2 decode-palmdoc-attributes]
   [:version 2 byte-array-int]
   [:creation-date 4 as-date]
   [:modification-date 4 as-date]
   [:backup-date 4 as-date]
   [:modification-number 4 byte-array-int]
   [:appinfo-offset 4 byte-array-int]
   [:sortinfo-offset 4 byte-array-int]
   [:type 4 as-string]
   [:creator 4 as-string]
   [:seed-id 4 byte-array-int]
   [:next-record-id 4 byte-array-int]
   [:record-count 2 byte-array-int]])

(def record-attributes
  [[:data-offset 4 byte-array-int]
   [:attributes 1 decode-record-attributes]
   [:id 3 byte-array-int]])

(def palmdoc-attributes
  [[:compression 2 byte-array-int]])

(defn decode-mobi [input-stream]
  (let [pdb-header (decode-attributes pdb-attributes input-stream)
        record-count (:record-count pdb-header)
        record-list (decode-record-info record-attributes record-count input-stream)
        palmdoc-header (decode-attributes palmdoc-attributes input-stream)]
    (-> pdb-header
      (assoc :record-list record-list)
      (assoc :palmdoc-header palmdoc-header))))

