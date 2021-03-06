(ns cloji.huff-cdic
  (:use [cloji.core]))

(defn unpack [coll huff cdic]
  (let [bl (* (count coll) 8)
        data (into (vec coll) (take 8 (repeat 0)))]
    (loop [out (str)
           bitsleft bl
           n 32
           pos 0
           dictionary cdic]
      (let [x (unpack-type data byte-array-int 8 pos)
            code (bit-and (bit-shift-right x n) (- (bit-shift-left 1 32) 1))
            [codelen term maxcode] (nth (:meta-info huff) (bit-shift-right code 24))
            len (if (= 0 term)
                  (+ codelen (count (take-while
                           #(< code (nth (map first (:limits huff)) %))
                           (iterate inc codelen))))
                  codelen)
            new-max (if (= 0 term)
                      (nth (map second (:limits huff)) len)
                      maxcode)
            r (bit-shift-right (- new-max code) (- 32 len))
            [slice flag] (nth dictionary r)]
        (if (< bitsleft 0)
          out
          (recur
            (str out slice)
            (- bitsleft len)
            (if (<= n 0)
              (- (+ n 32) len)
              (- n len))
            (if (<= n 0)
              (+ pos 4)
              pos)
            (if (= 0 flag)
              (assoc dictionary r [(unpack (map #(bit-and 0xff %) (.getBytes slice "CP1252")) huff dictionary) 1])
              dictionary)))))))

(defn- limit-unpack [coll]
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
      (map vector mincode maxcode))))

(defn- meta-unpack [x]
  (let [codelen (bit-and x 0x1F)
        term (bit-and x 0x80)
        maxcode (dec (bit-shift-left (inc (bit-shift-right x 8)) (- 32 codelen)))]
    [codelen, term, maxcode]))

(defn huff-table [table]
  (let [meta-offset (unpack-type table byte-array-int 4 8)
        limit-offset (unpack-type table byte-array-int 4 12)
        limit-coll (unpack-series table byte-array-int 64 4 limit-offset)]
    (merge {:limits (limit-unpack limit-coll)} {:meta-info (map meta-unpack
      (unpack-series table byte-array-int 256 4 meta-offset))})))

(defn cdic-table [headers is encoding]
  (let [first-rec (:first-huff-rec (:mobi-header headers))
        last-rec (+ first-rec (:huff-rec-count (:mobi-header headers)))
        get-slice
          (fn [rec off]
           (let [blen (unpack-type rec byte-array-int 2 (+ 16 off))]
             [((:decode mobi-string) (subvec (vec rec) (+ 18 off) (+ 18 off (bit-and blen 0x7FFF))) encoding) (bit-and blen 0x8000)]))]
    (loop [out []
           records (range (inc first-rec) last-rec)]
      (if-let [i (first records)]
        (let [cdic (read-record headers is i)
              phrases (unpack-type cdic byte-array-int 4 8)
              bits (unpack-type cdic byte-array-int 4 12)
              n (min (bit-shift-left 1 bits) (- phrases (count out)))]
          (recur (into out (map #(get-slice cdic %) (unpack-series cdic byte-array-int n 2 16))) (rest records)))
        out))))

(defn clean-cdic [huff cdic encoding]
  (map (fn [v]
         (let [[slice flag] v]
           (if (= 0 flag)
             [(unpack (map #(bit-and 0xff %) (.getBytes slice encoding)) huff cdic) 1]
             [slice flag])))
       cdic))
