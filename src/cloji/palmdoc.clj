(ns cloji.palmdoc)

(defn decomp-palmdoc
  "Takes a stream of bytes compressed using the Palmdoc compression algorithm
  and returns a sequence of decompressed bytes"
  [c]
  (loop [cs c us []]
    (let [nc (first cs)]
      (cond
        (= nil nc) us
        (= 0x00 nc) (recur (rest cs) (conj us nc))
        (and (<= 0x01 nc) (>= 0x08 nc)) (recur (drop (+ 1 nc) cs) (into us (take nc (rest cs))))
        (and (<= 0x09 nc) (>= 0x7F nc)) (recur (rest cs) (conj us nc))
        (and (<= 0x80 nc) (>= 0xBF nc))
          (if-let [nnc (nth cs 1 nil)]
            (let [lz7 (bit-and 0x3FFF (bit-or (bit-shift-left nc 8) nnc))
                  distance (bit-shift-right lz7 3)
                  length (+ 3 (bit-and 7 lz7))
                  lz7-decode (fn []
                               (loop [coll us n 0]
                                 (if (= n length)
                                   coll
                                   (recur (conj coll (nth coll (- (count coll) distance))) (inc n)))))]
              (recur (drop 2 cs) (lz7-decode))))
        (and (<= 0xC0 nc) (>= 0xFF nc)) (recur (rest cs) (into us [32 (bit-xor nc 0x80)]))))))

