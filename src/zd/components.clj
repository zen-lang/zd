(ns zd.components
  (:require
   [zd.zentext :as zentext]
   [zd.link :as link]
   [stylo.core :refer [c]]
   [zd.memstore :as memstore]
   [clojure.string :as str]
   [zd.methods :as methods]))

(defn table
  "renders table from vector of hashmaps. each hashmap is a memstore document"
  [ztx ctx headers data]
  [:table {:class (c :rounded [:py 2] [:w-max "80rem"]
                     {:display "block" :table-layout "fixed"})}
   [:thead
    (->> headers
         (map (fn [k]
                [:th {:class (c [:px 4] [:py 2] :border [:bg :gray-100])}
                 (str/lower-case (name k))]))
         (into [:tr]))]
   (->> data
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
                                  :class (c :inline-flex :items-center [:text "#4B5BA0"] [:hover [:underline]])}
                              (link/icon ztx doc)
                              (or (:title doc) docname)]

                             (= :zentext (:zd/content-type key-ann))
                             [:div {:class (c [:w-min "16rem"] :text-sm)}
                              (zentext/parse-block ztx v block)]

                             (= :edn (:zd/content-type key-ann))
                             (cond
                               (set? v)
                               (->> v
                                    (mapv (fn [e]
                                            (if (symbol? e)
                                              (link/symbol-link ztx e)
                                              (zentext/parse-block ztx (str e) block))))
                                    (into [:div {:class (c :flex :flex-col :text-sm {:flex-wrap "wrap"})}]))

                               :else (methods/rendercontent ztx ctx block))

                             (some? v)
                             (methods/rendercontent ztx ctx {:data v :key h :ann {:zd/content-type :edn}})))]))]))
        (into [:tbody]))])
