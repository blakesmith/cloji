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

(defn decode-trail-size [flags data]
  (loop [f flags size 0]
    (let [flag (last f)]
      (if (= (count f) 1)
        (if flag (+ (bit-and (last (drop-last size data)) 0x3) size) size)
        (recur (drop-last f) (if flag
                          (+ size (bvw-int (drop-last size data))) size))))))

(defn decode-record-info [attrs x input-stream]
  (doall (map (fn [_]
    (decode-attributes attrs input-stream)) (range x))))

(defn read-attributes [attrs input-stream offset]
  (with-location offset input-stream
    (decode-attributes attrs input-stream)))

(defn decode-record [headers input-stream n & [decomp-f]]
  (let [record (nth (:record-list headers) n)
        next-record (nth (:record-list headers) (inc n))
        f (or decomp-f palmdoc-string)
        read-size (- (:data-offset next-record) (:data-offset record))
        encoding (encoding-string (:encoding (:mobi-header headers)))
        data (with-location (:data-offset record) input-stream
              (read-bytes input-stream read-size nil))
        trail-size (decode-trail-size (bitset (:extra-flags (:mobi-header headers))) data)]
    (f (drop-last trail-size data) encoding)))

(defn decode-headers [input-stream]
  (with-location 0 input-stream
    (let [pdb-header (decode-attributes pdb-attributes input-stream)
          record-count (:record-count pdb-header)
          record-list (decode-record-info record-attributes record-count input-stream)
          first-offset (:data-offset (first record-list))
          palmdoc-header
            (read-attributes palmdoc-attributes input-stream first-offset)
          mobi-header (decode-attributes mobi-attributes input-stream)
          extra-flags
            (if (or (= 0xE4 (:header-length mobi-header))
                    (= 0xE8 (:header-length mobi-header)))
              (read-attributes extra-flag-attributes input-stream (+ first-offset 0xF2))
              0)]
      (-> pdb-header
        (assoc :record-list record-list)
        (assoc :palmdoc-header palmdoc-header)
        (assoc :mobi-header (conj extra-flags mobi-header))))))

(defn decode-body [input-stream]
  (with-location 0 input-stream
    (let [headers (decode-headers input-stream)]
      (reduce str
        (map #(decode-record headers input-stream %)
             (range 1 (inc (:record-count (:palmdoc-header headers)))))))))

