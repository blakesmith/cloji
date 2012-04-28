(ns cloji.decoder
  (:use [cloji.core]
        [cloji.attributes])
  (:require [clojure.java.io :as io])
  (:import [javax.imageio ImageIO]))

(defn- bytes-from-attributes [attrs is]
  (read-bytes is (header-length attrs)))

(defn attribute-mappings [attrs b maps]
  (if (zero? (count attrs))
    maps
    (let [{:keys [field type len skip]} (first attrs)]
      (recur (rest attrs)
             (drop (+ len (or skip 0)) b)
             (assoc maps field ((:decode type) (take len (drop (or skip 0) b))))))))

(defn decode-attributes [attrs is]
  (attribute-mappings attrs (bytes-from-attributes attrs is) {}))

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
        f (:decode (get compression-fn (:compression (:palmdoc-header headers))))
        data (read-record headers is n)
        trail-size (decode-trail-size (bitset (:extra-flags (:mobi-header headers))) data)]
    (f headers is (drop-last trail-size data) encoding)))

(defn- decode-full-name [is offset length]
  (let [attrs (assoc (first full-name-attributes) :len length)]
    (read-attributes [attrs] is offset)))

(defn- decode-exth-record [is]
  (let [type ((:decode exth-type) (read-bytes is 4))
        len (- ((:decode byte-array-int) (read-bytes is 4)) 8)
        value ((:decode mobi-string) (read-bytes is len))]
    [type value]))

(defn- extract-pdb-header [headers attrs is]
  (conj headers (decode-attributes attrs is)))

(defn- extract-record-list [headers attrs is]
  (assoc headers :record-list
         (decode-record-info attrs (:record-count headers) is)))

(defn- extract-palmdoc-header [headers attrs is]
  (assoc headers :palmdoc-header
         (read-attributes attrs is (:data-offset (first (:record-list headers))))))

(defn- extract-mobi-header [headers attrs is]
  (assoc headers :mobi-header (decode-attributes attrs is)))

(defn- extract-exth-header [headers attrs is]
  (assoc headers :exth-header
         (read-attributes attrs is (+ (:data-offset (first (:record-list headers)))
                                      (:header-length (:mobi-header headers)) 16))))

(defn- extract-exth-records [headers is]
  (assoc headers :exth-records
         (reduce conj {}
                 (take (:record-count (:exth-header headers))
                       (repeatedly (partial decode-exth-record is))))))

(defn- extract-extra-flags [headers attrs is]
  (let [mobi-header (:mobi-header headers)]
    (assoc headers :mobi-header
           (merge mobi-header
                  (if (or (= 0xE4 (:header-length mobi-header))
                          (= 0xE8 (:header-length mobi-header)))
                    (read-attributes attrs is (+ (:data-offset (first (:record-list headers))) 0xF2))
                    {:extra-flags 0})))))

(defn- extract-full-name [headers is]
  (let [mobi-header (:mobi-header headers)]
    (into headers
          (decode-full-name
           is
           (+ (:full-name-offset mobi-header) (:data-offset (first (:record-list headers))))
           (:full-name-length mobi-header)))))

(defn decode-headers [is]
  "Takes a mobipocket RandomAccessFile and decodes the mobipocket headers, returns a map of header attributes that are necessary for decoding the body and extracting images"
  (with-location 0 is
    (-> {}
        (extract-pdb-header pdb-attributes is)
        (extract-record-list record-attributes is)
        (extract-palmdoc-header palmdoc-attributes is)
        (extract-mobi-header mobi-attributes is)
        (extract-exth-header exth-attributes is)
        (extract-exth-records is)
        (extract-extra-flags extra-flag-attributes is)
        (extract-full-name is))))

(defn decode-image [headers is n]
  "Returns a BufferedImage from the mobi image record at offset n"
  (let [ri (record-info headers is (+ n (:first-image-offset (:mobi-header headers))))
        b (byte-array (:read-size ri))]
    (with-location (:seek ri) is
      (do
        (.read is b 0 (:read-size ri))
        (ImageIO/read (io/input-stream b))))))

(defn decode-images [headers is]
  "Return a list of buffered images for the mobi file"
  (take-while identity (map #(decode-image headers is %) (iterate inc 1))))

(defn decode-to-file [is out]
  "Top level function to decode all text records and write them to a file"
  (with-location 0 is
    (let [headers (decode-headers is)]
      (with-open [os (io/writer out)]
        (doseq [n (range 1 (inc (:record-count (:palmdoc-header headers))))]
          (.write os (decode-record headers is n)))))))

(defn decode-mobi [is]
  "Top level function to decode all text records and concatenate them together"
  (with-location 0 is
    (let [headers (decode-headers is)]
      {:headers headers
      :body (reduce str
        (map #(decode-record headers is %)
             (range 1 (inc (:record-count (:palmdoc-header headers))))))})))

