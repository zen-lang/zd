(ns zd.blocks.zd
  (:require [zd.link :as link]
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

             (sort-by :doc)
             (group-by :path))]
    [:div {:class (c [:text :gray-600])}
     [:div {:class (c :flex :flex-row [:text :gray-600] :justify-between :border-b)}
      [:div
       [:span ":backlinks"]]
      [:div
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

      (doall
       (for [[p ls] links]
         [:div
          [:div {:class (c [:py 1])} p]
          [:div
           (doall
            (for [{:keys [to doc path]} ls]
              [:div {:class (str "badge " (name (c :border [:my 1] [:mr 2]  :inline-flex :rounded [:p 0])))}
               [:div {:class (c [:px 2] [:py 0.5] :inline-block :text-sm)}
                (link/symbol-link ztx doc)]]))]]))]]))

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
