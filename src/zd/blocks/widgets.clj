(ns zd.blocks.widgets
  (:require
   [zd.db :as db]
   [clojure.string :as str]
   [zd.memstore :as memstore]
   [zd.link :as link]
   [zd.methods :as methods]
   [zd.utils :as utils]
   [stylo.core :refer [c]]
   [zd.meta :as meta]))

(defn render-desc [ztx anns desc]
  [:div {:class (c [:text :gray-600] [:py 1])}
   (let [text-limit 128
         desc-text
         (->> (map-indexed vector (str desc))
              (take-while (fn [[i ch]]
                            (or (< i text-limit)
                                (and (>= i text-limit)
                                     (not= ch \.)))))
              (map second)
              (apply str))
         block {:key :desc :data desc-text :ann (get anns :desc)}]
     (methods/rendercontent ztx {} block))])


