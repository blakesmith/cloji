(ns cloji.decoder
  (:use [cloji.core])
  (:use [cloji.attributes]))

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

(defn decomp-palmdoc
  "Takes a stream of bytes compressed using the Palmdoc compression algorithm
  and returns a sequence of decompressed bytes"
  [c]
  (loop [cs c us [] pos 0]
    (let [nc (first cs)]
      (cond
        (= nil nc) us
        (= 0x00 nc) (recur (rest cs) (conj us nc) (inc pos))
        (and (<= 0x01 nc) (>= 0x08 nc)) (recur (drop (+ 1 nc) cs) (concat us (take nc (rest cs))) (+ pos nc 1))
        (and (<= 0x09 nc) (>= 0x7F nc)) (recur (rest cs) (conj us nc) (inc pos))
        (and (<= 0x80 nc) (>= 0xBF nc))
          (let [distance (bit-xor 1024 (bit-or (bit-shift-left nc 3) (bit-shift-right (nth cs 1) 5)))
                length (bit-and 7 (nth c 1))]
            (recur (drop 2 cs) (concat us (take length (drop (- pos distance) us))) (+ pos 2)))
        (and (<= 0xC0 nc) (>= 0xFF nc)) (recur (rest cs) (concat us [\space (bit-xor nc 0x80)]) (inc pos))))))

(defn decode-mobi [input-stream]
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

