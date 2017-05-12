(ns clojure-noob.api
  (:require [clj-http.client :as client]
            [cheshire.core :refer :all]))

(def prefix "https://deckofcardsapi.com/api/deck/")

(defn get-urls
  ([] {:new (str prefix "new/shuffle/?deck_count=1")})
  ([id] (into (get-urls) {:shuffle (str prefix id "/shuffle")}))
  ([id count] (into (get-urls id) {:draw (str prefix id "/draw/?count=" count)})))
