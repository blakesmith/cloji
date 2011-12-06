(ns cloji.decoder
  (:use [cloji.core]))

(defn decode-palmdoc-header [input-stream]
  (.seek input-stream 0)
  (as-string (read-bytes input-stream 32)))

(defn decode-palmdoc-attributes [input-stream]
  (bitfield (byte-array-int (read-bytes input-stream 2))
            {:res-db 0x0001
             :read-only 0x0002
             :appinfo-dirty 0x0004
             :backup 0x0008
             :install-newer 0x0010
             :reset 0x0020
             :no-copy 0x0040}))

(defn decode-mobi [input-stream]
  {:name (decode-palmdoc-header input-stream)
   :attributes (decode-palmdoc-attributes input-stream)})
