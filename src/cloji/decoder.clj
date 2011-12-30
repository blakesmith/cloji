(ns cloji.decoder
  (:use [cloji.core])
  (:use [cloji.attributes]))

(def encoding-string
  {:utf-8 "UTF-8"
   :cp1252 "CP1252"})

(defn decode-attributes [attrs input-stream]
  (into {}
    (for [[attr-name f len skip] attrs]
      [attr-name (f (read-bytes input-stream len skip))])))

(defmacro with-location [l input-stream body]
  `(do (.seek ~input-stream ~l)
    ~body))

(defn decode-record-info [attrs x input-stream]
  (doall (map (fn [_]
    (decode-attributes attrs input-stream)) (range x))))

(defn decode-record [headers input-stream n]
  (let [record (nth (:record-list headers) n)
        next-record (nth (:record-list headers) (inc n))
        read-size (+ (- (:data-offset next-record) (:data-offset record) 1))
        encoding (encoding-string (:encoding (:mobi-header headers)))]
    (with-location (:data-offset record) input-stream
      (palmdoc-string (read-bytes input-stream read-size nil) encoding))))

(defn decode-headers [input-stream]
  (let [pdb-header (decode-attributes pdb-attributes input-stream)
        record-count (:record-count pdb-header)
        record-list (decode-record-info record-attributes record-count input-stream)
        first-offset (:data-offset (first record-list))
        palmdoc-header
          (with-location first-offset input-stream
             (decode-attributes palmdoc-attributes input-stream))
        mobi-header (decode-attributes mobi-attributes input-stream)]
    (-> pdb-header
      (assoc :record-list record-list)
      (assoc :palmdoc-header palmdoc-header)
      (assoc :mobi-header mobi-header))))

