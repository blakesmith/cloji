(ns cloji.decoder
  (:use [cloji.core])
  (:use [cloji.attributes])
  (:import [javax.imageio ImageIO]))

(def encoding-string
  {:utf-8 "UTF-8"
   :cp1252 "CP1252"})

(def compression-fn
  {1 (fn [coll encoding] (as-string coll encoding))
   2 palmdoc-string
   17480 huffman-string})

(defn huff-table [table]
  (let [meta-offset (unpack-type table byte-array-int 4 8)
        limit-offset (unpack-type table byte-array-int 4 12)
        limit-coll (unpack-series table byte-array-int 64 4 limit-offset)
        meta-unpack
          (fn [x]
            (let [codelen (bit-and x 0x1F)
                  term (bit-and x 0x80)
                  maxcode (dec (bit-shift-left (inc (bit-shift-right x 8)) (- 32 codelen)))]
              [codelen, term, maxcode]))
        limit-unpack
          (fn [coll]
            (loop [in (into [0 0] coll)
                   pos 0
                   mincode []
                   maxcode []]
              (if-let [x (first in)]
                (cond
                  (even? pos)
                    (recur
                      (rest in) (inc pos) (conj mincode (bit-shift-left x (- 32 (count mincode)))) maxcode)
                  (odd? pos)
                    (recur
                      (rest in) (inc pos) mincode (conj maxcode (dec (bit-shift-left (inc x) (- 32 (count maxcode)))))))
                {:limits (map vector mincode maxcode)}
                )))]

    (merge (limit-unpack limit-coll) {:meta-info (map meta-unpack
      (unpack-series table byte-array-int 256 4 meta-offset))})))

(defn cdic-table [headers is encoding]
  (let [first-rec (:first-huff-rec (:mobi-header headers))
        last-rec (+ first-rec (:huff-rec-count (:mobi-header headers)))
        get-slice
          (fn [rec off]
           (let [blen (unpack-type rec byte-array-int 2 (+ 16 off))]
             [(as-string (subvec (vec rec) (+ 18 off) (+ 18 off (bit-and blen 0x7FFF))) encoding) (bit-and blen 0x8000)]))]
    (loop [out []
           records (range (inc first-rec) last-rec)]
      (if-let [i (first records)]
        (let [cdic (read-record headers is i)
              phrases (unpack-type cdic byte-array-int 4 8)
              bits (unpack-type cdic byte-array-int 4 12)
              n (min (bit-shift-left 1 bits) (- phrases (count out)))]
          (recur (into out (map #(get-slice cdic %) (unpack-series cdic byte-array-int n 2 16))) (rest records)))
        out))))

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
    (f (drop-last trail-size data) encoding)))

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

