(ns cloji.core
  (:import [java.util Date]))

(defmacro with-location [l is body]
  `(do (.seek ~is ~l)
       ~body))

(defn unpack-type [coll f size & [offset]]
  ((:decode f) (take size (if offset (drop offset coll) coll))))

(defn unpack-series [coll f n size & [offset]]
  (map #(unpack-type coll f size (+ offset (* size %))) (range n)))

(def byte-array-int
  {:decode (fn [coll]
             (reduce +
                     (map (fn [b i]
                            (bit-shift-left b (* i 8)))
                          coll (iterate dec (- (count coll) 1)))))
   :encode (fn [v len]
             (map (fn [i]
                    (bit-and 0xff (.byteValue (bit-and (bit-shift-right v i) 0xff))))
                  (range (* (dec len) 8) -8 -8)))})

(defn packed-int [v n]
  ((:encode byte-array-int) v n))

(defn bvw-int [data]
  "Backwards encoded variable width integer"
  (loop [d data bit-pos 0 result 0]
    (let [v (last d)
          r (bit-or result (bit-shift-left (bit-and v 0x7F) bit-pos))]
      (if (or (not= (bit-and v 0x80) 0)
              (>= bit-pos 28)
              (nil? v))
        r
        (recur (drop-last d) (+ bit-pos 7) r)))))


(def bitfield
  {:decode (fn [value mappings]
             (map first
                  (filter #(< 0 (bit-and (last %) value)) mappings)))
   :encode (fn [values mappings]
             (reduce bit-or 0 (map #(% mappings) values)))})

(defn bitset [value & [max-count]]
  (map (fn [i]
    (= (bit-and (bit-shift-right value i) 1) 1))
    (range (or max-count 16))))

(defn read-bytes [is n]
  (vec (filter #(not= -1 %) (take n (repeatedly #(.read is))))))

(defn record-info [headers n]
  "Helper function to retrieve the position offset in the file and read size for a record at index n"
  (let [record (nth (:record-list headers) n)
        next-record (nth (:record-list headers) (inc n))]
    {:read-size (- (:data-offset next-record) (:data-offset record))
     :seek (:data-offset record)}))

(defn read-record [headers is n]
  "Read the raw data in a record"
  (let [ri (record-info headers n)]
    (with-location (:seek ri) is
      (read-bytes is (:read-size ri)))))

(def mobi-string
  {:decode (fn [coll & [encoding]]
             (let [e (or encoding "UTF-8")]
               (String. (into-array Byte/TYPE (map #(.byteValue %) (filter (complement zero?) coll))) e)))
   :encode (fn [v len & [encoding]]
             (let [b (vec (.getBytes v (or encoding "UTF-8")))]
               (into b (take (- len (count b)) (repeat 0)))))})

(def mobi-date
  {:decode (fn [coll]
             (let [t ((:decode byte-array-int) coll)]
               (when (not (= 0 t))
                 (doto (new Date) (.setTime (* 1000 t))))))
   :encode (fn [v len]
             (if (nil? v)
               (take len (repeat 0))
               ((:encode byte-array-int) (quot (.getTime v) 1000) len)))})




