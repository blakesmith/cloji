(ns cloji.decoder
  (:use [cloji.core]))

(defn type-lookup [types coll]
  (get types (byte-array-int coll)))

(defn mobi-type [coll]
  (type-lookup
    {2 :mobi-book
     3 :palmdoc-book
     4 :audio
     257 :news
     258 :news-feed
     259 :news-magazine
     513 :pics
     514 :world
     515 :xls
     516 :ppt
     517 :text
     518 :html} coll))

(defn decode-palmdoc-attributes [coll]
  (bitfield (byte-array-int coll)
            {:res-db 0x0001
             :read-only 0x0002
             :appinfo-dirty 0x0004
             :backup 0x0008
             :install-newer 0x0010
             :reset 0x0020
             :no-copy 0x0040}))

(defn decode-record-attributes [coll]
  (bitfield (byte-array-int coll)
            {:secret 0x0010
             :in-use 0x0020
             :dirty 0x0040
             :delete 0x0080}))

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

(def pdb-attributes
  [[:name as-string 32]
   [:attributes decode-palmdoc-attributes 2]
   [:version byte-array-int 2]
   [:creation-date as-date 4]
   [:modification-date as-date 4]
   [:backup-date as-date 4]
   [:modification-number byte-array-int 4]
   [:appinfo-offset byte-array-int 4]
   [:sortinfo-offset byte-array-int 4]
   [:type as-string 4]
   [:creator as-string 4]
   [:seed-id byte-array-int 4]
   [:next-record-id byte-array-int 4]
   [:record-count byte-array-int 2]])

(def record-attributes
  [[:data-offset byte-array-int 4]
   [:attributes decode-record-attributes 1]
   [:id byte-array-int 3]])

(def palmdoc-attributes
  [[:compression byte-array-int 2]
   [:text-length byte-array-int 4 2]
   [:record-count byte-array-int 2]
   [:record-size byte-array-int 2]
   [:current-position byte-array-int 4]])

(def mobi-attributes
  [[:header-length byte-array-int 4 4]
   [:mobi-type mobi-type 4]])

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

