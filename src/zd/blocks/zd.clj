(ns zd.blocks.zd
  (:require [zd.link :as link]
            [zd.zentext :as zentext]
            [zd.memstore :as memstore]
            [zd.meta :as meta]
            [clojure.string :as str]
            [stylo.core :refer [c]]
            [zd.methods :as methods]))

(defmethod methods/renderkey :zd/docname
  [ztx ctx {data :data :as block}]
  [:div {:class (str "badge " (name (c :border [:my 1] [:mr 2]  :inline-flex :rounded [:p 0])))}
   [:div {:class (c :inline-block [:px 2] [:bg :gray-100] [:py 0.5] :text-sm [:text :gray-700]
                    {:font-weight "400" :padding-top ".3rem"})}
    [:i.fas.fa-file]]
   [:div {:class (c [:px 2] [:py 0.5] :inline-block :text-sm)}
    data]])

(defn docs-cards [ztx ctx docs]
  (let [summary-keys (meta/get-group ztx :zd/summary)]
    [:div
   ;; TODO remove last-updated from db query?
     (for [[p links] (group-by :path docs)]
       [:div {:class (c [:mt 4])}
        [:span p]
        (for [{docname :doc} links]
          (let [{{anns :ann lu :last-updated} :zd/meta :as doc}
                (memstore/get-doc ztx (symbol docname))]
            [:div {:class (c [:py 2] [:my 2] :flex :flex-col [:border-b "0.5" :gray-200])}
             [:div {:class (c :inline-flex)}
              (link/symbol-link ztx docname)
              [:div {:class (c :flex :self-center)}
               (when (str/includes? (str docname) "_template")
                 [:span {:class (c [:text :orange-500] [:py 1] [:px 2])}
                  "_template"])
               (when (str/includes? (str docname) "_schema")
                 [:span {:class (c [:text :orange-500] [:p 1] [:px 2])}
                  "_schema"])
               ;; TODO get last updated from git repo
               #_[:div {:class (c [:text :gray-500])}
                  "upd: " lu]]]
             [:div
              (doall
               (for [[k v] (select-keys doc summary-keys)]
                 (when (= (get-in anns [k :zd/content-type]) :edn)
                   [:div {:class (c :inline-flex :text-sm :items-baseline [:mr 2.2])}
                    [:div {:class (c [:mr 0.5])}
                     (str (name k) ":")]
                    [:div {:class (c :flex :flex-no-wrap :overflow-hidden [:mr 1])}
                     (cond
                       (or (set? v) (vector? v))
                       (into [:div {:class (c :flex [:space-x 1] :items-center)}]
                             (interpose
                              [:span {:class (c [:m 0] [:p 0])} ","]
                              (mapv
                               (fn [s]
                                 (if (symbol? s)
                                   (let [res (memstore/get-doc ztx s)]
                                     [:a {:href (str "/" s)
                                          :class (c :inline-flex
                                                    :items-center
                                                    [:hover [:text :blue-600] :underline]
                                                    :whitespace-no-wrap
                                                    {:text-decoration-thickness "0.5px"})}
                                      [:span (:title res)]])
                                   [:span (pr-str s)]))
                               v)))

                       (string? v)
                       (zentext/parse-block ztx v {:key k :data v :ann (get anns k)})

                       :else [:span (pr-str v)])

                     #_(methods/rendercontent ztx ctx {:key k :data v :ann (get anns k)})]])))]]))])]))

(defmethod methods/renderkey :zd/backlinks
  [ztx {{{dn :docname} :zd/meta} :doc {qs :query-string} :request r :root :as ctx} {:keys [data] :as block}]
  (let [links
        (->> data
             (map (fn [{d :doc p :path t :to}]
                    {:to t
                     :doc d
                     :path (->> (map name p)
                                (str/join ".")
                                (str ":"))}))

             (sort-by (juxt :path :doc))
             ;; TODO impl paging for backlinks?
             #_(take 50))]
    [:div {:class (c [:text :gray-600] [:pt 2])}
     [:div {:class (c :flex :flex-row [:text :gray-600] :border-b :items-baseline :justify-between)}
      [:div {:class (c :flex :items-center)}
       [:a {:id "backlinks"}
        [:span {:class (c :text-sm {:text-transform "uppercase"})}
         ":backlinks"]]]
      [:div {:class (c [:pl 2])}
       [:a {:class (c :cursor-pointer [:text :gray-600] [:hover [:text :green-600]])
            :href (cond->> (str "_draft/edit?" qs)
                    (not= dn (symbol r)) (str dn "."))
            ;; TODO impl create by backlink flow
            #_:onclick #_"create_redirect()"}
        [:i.fas.fa-plus]]
       [:select#zd-select {:class (c :text-base)}
        [:option {:value "parent"} ":parent"]]]
      #_[:div {:class (c [:text :gray-500])}
         [:span "zdkey"]]]
     [:div {:class (c [:py 2])}
      (docs-cards ztx ctx links)]]))

(defmethod methods/renderkey :zd/errors
  [ztx ctx {errors :data k :key :as block}]
  (when (seq errors)
    [:div {:class (c [:text :red-700] [:p 4] [:my 4] :rounded
                     {:background-color "#fff5f5"})}
     [:ul {:class (c :font-bold :text-lg [:mb 2] [:ml 0] {:color "#e53e3e"})}
      "Document errors"]
     (for [err (sort-by :type errors)]
       [:li {:class (c [:mb 1] [:py 1] :flex [:space-x 3] [:text :gray-600])}
        [:span (->> (:path err)
                    (map (fn [p] (if (keyword? p) (name p) (str p))))
                    (str/join ".")
                    (str ":"))]
        [:span {:class (c [:ml 4] {:text-align "right"})} (:message err)]])]))
