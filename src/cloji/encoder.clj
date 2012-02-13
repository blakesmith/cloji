(ns cloji.encoder
  (:use [cloji.core]
        [cloji.attributes])
  (:require [cloji.palmdoc :as palmdoc]))

(defn compressed-palmdoc [str]
  (if (> (count str) 4096)
    (throw (IllegalArgumentException. "Palmdoc strings must be less than 4096 characters in length"))))