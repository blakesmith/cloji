(ns cloji.encoder
  (:use [cloji.core]
        [cloji.attributes])
  (:require [cloji.palmdoc :as palmdoc]))

(defn compressed-palmdoc [str]
  {:pre [(<= (count str) 4096)]} nil)