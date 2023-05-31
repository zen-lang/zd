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

(defmethod methods/renderkey :zd/backlinks
  [ztx {{{dn :docname} :zd/meta} :doc {qs :query-string} :request r :root :as ctx} {:keys [data] :as block}]
  (let [summary-keys (meta/get-group ztx :zd/summary)
        links
        (->> data
             (map (fn [{d :doc p :path t :to}]
                    {:to t
                     :doc d
                     :parent (when (str/includes? (str d) ".")
                               (str/join "." (butlast (str/split (str d) #"\."))))
                     :path (->> (map name p)
                                (str/join ".")
                                (str ":"))}))
             (sort-by (juxt :path :doc))
             (group-by :parent))]
    (for [[parent links] links]
      (let [*parent (or parent r)]
        [:div {:class (c [:py 4] [:text :gray-600] )}
         ;; TODO think about prefixed anchor e.g. :backlinks/parent
         [:div {:class (c :flex :flex-row :justify-between :items-baseline :text-sm)}
          [:a {:id *parent :class (c :uppercase)}
           [:span {:class (c :text-xs [:pr 0.5] [:text :green-300])}
            [:i.fas.fa-link]]
           *parent]
          [:div {:class (c [:text :gray-500])}
           [:span {:class (c [:pr 2])}
            (str/join ", " (set (map :path links)))]
           [:a {:class (c :cursor-pointer [:hover [:text :green-600]])
                :href (if (some? parent)
                        (str parent "." "_draft/edit")
                        "_draft/edit")}
            [:i.fas.fa-plus]]]]
         (for [{p :path docname :doc} links]
           (let [{{anns :ann lu :last-updated} :zd/meta :as doc}
                 (memstore/get-doc ztx (symbol docname))]
             [:div {:class (c [:pt 4] :flex :flex-col)}
              [:div {:class (c :inline-flex :items-center)}
               (link/symbol-link ztx docname)
               #_[:span {:class (c :text-xs [:pl 2])} p]
               [:div {:class (c :flex :self-center)}
                (when (str/includes? (str docname) "_template")
                  [:span {:class (c :text-xs [:text :orange-500] [:pl 2])}
                   "_template"])
                (when (str/includes? (str docname) "_schema")
                  [:span {:class (c :text-xs [:text :orange-500] [:pl 2])}
                   "_schema"])
               ;; TODO get last updated from git repo
                #_[:div {:class (c [:text :gray-500])}
                   "upd: " lu]]]
              [:div {:class (c :flex :flex-no-wrap :overflow-x-hidden)}
               (doall
                (for [[k v] (select-keys doc summary-keys)]
                  (when (= (get-in anns [k :zd/content-type]) :edn)
                    [:div {:class (c :inline-flex :text-sm :items-center [:mr 2.2])}
                     [:div {:class (c [:mr 0.5])}
                      (str (name k) ":")]
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

                       (symbol? v)
                       (let [res (memstore/get-doc ztx v)]
                         [:a {:href (str "/" v)
                              :class (c :inline-flex
                                        :items-center
                                        [:hover [:text :blue-600] :underline]
                                        :whitespace-no-wrap
                                        {:text-decoration-thickness "0.5px"})}
                          [:span (:title res)]])

                       (string? v)
                       (zentext/parse-block ztx v {:key k :data v :ann (get anns k)})

                       :else [:span (pr-str v)])])))]]))]))))

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
