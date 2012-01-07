(ns cloji.decoder
  (:use [cloji.core])
  (:use [cloji.attributes])
  (:import [javax.imageio ImageIO]))

(def encoding-string
  {:utf-8 "UTF-8"
   :cp1252 "CP1252"})

(defn decode-attributes [attrs is]
  (into {}
    (for [[attr-name f len skip] attrs]
      [attr-name (f (read-bytes is len skip))])))

(defmacro with-location [l is body]
  `(do (.seek ~is ~l)
    ~body))

(defn decode-trail-size [flags data]
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

(defn record-info [headers n]
  (let [record (nth (:record-list headers) n)
        next-record (nth (:record-list headers) (inc n))]
    {:read-size (- (:data-offset next-record) (:data-offset record))
     :seek (:data-offset record)}))


(defn decode-record [headers is n & [decomp-f]]
  (let [ri (record-info headers n)
        f (or decomp-f palmdoc-string)
        encoding (encoding-string (:encoding (:mobi-header headers)))
        data (with-location (:seek ri) is
              (read-bytes is (:read-size ri) nil))
        trail-size (decode-trail-size (bitset (:extra-flags (:mobi-header headers))) data)]
    (f (drop-last trail-size data) encoding)))

(defn decode-headers [is]
  (with-location 0 is
    (let [pdb-header (decode-attributes pdb-attributes is)
          record-count (:record-count pdb-header)
          record-list (decode-record-info record-attributes record-count is)
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
  (let [index (+ n (:first-image-offset (:mobi-header headers)))
        ri (record-info headers index)
        b (byte-array (:read-size ri))]
    (with-location (:seek ri) is
      (do
        (.read is b 0 (:read-size ri))
        (ImageIO/read (clojure.java.io/input-stream b))))))

(defn decode-body [is]
  (with-location 0 is
    (let [headers (decode-headers is)]
      (reduce str
        (map #(decode-record headers is %)
             (range 1 (inc (:record-count (:palmdoc-header headers)))))))))

