(ns zd.components
  (:require
   [zd.zentext :as zentext]
   [zd.link :as link]
   [stylo.core :refer [c]]
   [zd.memstore :as memstore]
   [clojure.string :as str]
   [zd.methods :as methods]))

(defn table
  "renders table from vector of hashmaps. memstore document model is supported."
  [ztx ctx headers data]
  [:table {:class (c :rounded [:py 2]
                     {:display "block"
                      :table-layout "fixed"
                      :overflow-x "overlay"})}
   [:thead
    (->> headers
         (map (fn [k]
                [:th {:class (c [:px 4] [:py 2] :border [:bg :gray-100])}
                 (str/lower-case (name k))]))
         (into [:tr]))]
   (->> data
        (sort-by (fn [r]
                   (if-let [v (get r (first headers))]
                     (if (string? v)
                       v
                       (pr-str v)))))
        (mapv (fn [row]
                [:tr
                 (doall
                  (for [h headers]
                    [:td {:class (c [:px 4] [:py 2] :border {:vertical-align "top"})}
                     (let [v (get row h)
                           docname (get row :xt/id)
                           {{anns :ann} :zd/meta :as doc}
                           (when docname
                             (memstore/get-doc ztx (symbol docname)))
                           key-ann (get anns h)
                           block {:key h :data v :ann key-ann}]

                       (cond (= :xt/id h)
                             [:a {:href (str "/" docname)
                                  :class (c :inline-flex :items-center [:text :blue-600] [:hover [:underline]])}
                              (link/icon ztx doc)
                              (or (:title doc) docname)]

                             (and (some? v) (= :zentext (:zd/content-type key-ann)))
                             [:div (zentext/parse-block ztx v block)]

                             ;; TODO think about saving symbols in xtdb
                             (and (some? v) docname)
                             (methods/rendercontent ztx ctx block)

                             (some? v)
                             (methods/rendercontent ztx ctx {:data v :key h :ann {:zd/content-type :edn}})))]))]))
        (into [:tbody]))])
