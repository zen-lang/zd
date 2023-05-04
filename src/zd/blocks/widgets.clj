(ns zd.blocks.widgets
  (:require
   [zd.db :as db]
   [clojure.string :as str]
   [zd.loader :as loader]
   [zd.link :as link]
   [zd.methods :as methods]
   [zd.utils :as utils]
   [stylo.core :refer [c]]))

(defn pagination [ztx
                  {{path "x-client-path"
                    qs "x-client-qs"} :headers :as req}
                  dn-param items-count page-number]
  (let [pages-count (+ (quot items-count 24)
                       (if (= 0 (rem items-count 24))
                         0
                         1))
        page-number* (if (some? page-number)
                       (read-string page-number)
                       1)]
    [:div {:class (c :flex :flex-row :justify-end [:text :gray-600] :text-sm)}
     (if (> items-count 0)
       [:div {:class (c :flex :flex-row :items-baseline)}
        [:a.fas.fa-regular.fa-arrow-left
         {:href (when (> page-number* 1)
                  (str path (utils/add-page-param qs (- page-number* 1)) ))
          :style {:font-size "14px" :padding "0 4px 0 4px" :cursor "pointer"}}]
        [:span "page " page-number*]
        [:span
         "/"]
        [:span pages-count]
        [:a.fas.fa-regular.fa-arrow-right
         {:href (when (< page-number* pages-count)
                  (str path (utils/add-page-param qs (+ page-number* 1))))
          :style {:font-size "14px" :padding "0 4px 0 4px" :cursor "pointer"}}]
        [:div {:class (c :flex :justify-center [:ml 2])}
         [:span "total: "]
         [:span items-count]]]
       [:div
        "Add a document with + button"])]))

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
     (let [{{anns :ann lu :last-updated} :zd/meta :as doc}
           (loader/get-doc ztx (symbol docname))]
       [:div {:class (c [:py 3])}
        [:div {:class (c :flex :flex-row :justify-between :border-b)}
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
        (when-let [desc (get doc :desc)]
          (render-desc ztx anns desc))
        (doall
         (for [[k v] (select-keys doc summary-keys)]
           (methods/renderkey ztx {} {:key k :data v :ann (get anns k)})))]))])

(defmethod methods/widget :folder
  [ztx {{{qs "x-client-qs"} :headers config :zd/config :as req} :request :as ctx}
   {{dn :docname} :zd/meta :as doc}]
  (let [{page-number :page search-text :search} (utils/parse-params qs)

        summary-keys
        (->> (get-in @ztx [:zd/schema])
             (filter (fn [[k v]]
                       (= :zd/summary (:group v))))
             (map first)
             (vec))
        dn-param (if (= (symbol (:root config)) dn)
                   ""
                   (str dn))
        query-result (db/children ztx dn-param page-number search-text)
        items-count (if-let [c (->> search-text
                                    (db/children-count ztx dn-param)
                                    (ffirst))]
                      c
                      0)]
    [:div
     (pagination ztx req dn-param items-count page-number)
     (docs-cards ztx ctx summary-keys query-result)]))
