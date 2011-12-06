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

(def mobi-attributes
  [[:name 32 as-string]
   [:attributes 2 decode-palmdoc-attributes]
   [:version 2 byte-array-int]])

(defn decode-mobi [input-stream]
  (into {}
    (for [[attr-name len f] mobi-attributes]
      [attr-name (f (read-bytes input-stream len))])))

