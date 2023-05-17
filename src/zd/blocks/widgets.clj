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

(defn docs-cards [ztx ctx summary-keys query-result]
  [:div
   ;; TODO remove last-updated from db query?
   (for [[i [docname _]] (map-indexed vector query-result)]
     (let [{{anns :ann lu :last-updated} :zd/meta p :parent :as doc}
           (memstore/get-doc ztx (symbol docname))]
       [:div {:class (c [:py 3])}
        [:div {:class (c :flex :flex-row :justify-between :border-b :overflow-hidden)}
         (link/symbol-link ztx docname)
         [:div {:class (c :flex :text-sm :self-center)}
          [:div {:class (c [:px 2])}
           (when (str/includes? (str docname) "_template")
             [:span {:class (c [:text :orange-500] [:py 1] [:px 2])}
              "_template"])
           (when (str/includes? (str docname) "_schema")
             [:span {:class (c [:text :orange-500] [:p 1] [:px 2])}
              "_schema"])]
          #_[:div {:class (c [:text :gray-500])}
             "upd: " lu]]]
        (when (symbol? p)
          [:div p])
        #_(when-let [desc (get doc :desc)]
            (render-desc ztx anns desc))
        #_[:div {:class (c :text-sm)}
           (doall
            (for [[k v] (select-keys doc summary-keys)]
              (methods/renderkey ztx {} {:key k :data v :ann (get anns k)})))]]))])
