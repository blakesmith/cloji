(ns cloji.decoder
  (:use [cloji.core]))

(defn decode-palmdoc-header [input-stream]
  (.seek input-stream 0)
  (as-string (read-bytes input-stream 32)))

(defn decode-mobi [input-stream]
  {:name (decode-palmdoc-header input-stream)})
