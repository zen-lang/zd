(ns zd.components
  (:require
   [zd.link :as link]
   [stylo.core :refer [c]]
   [zd.loader :as loader]
   [clojure.string :as str]
   [zd.methods :as methods]))

(defn table
  "renders table from vector of hashmaps. memstore document model is supported."
  [ztx ctx headers data]
  [:table {:class (c :shadow-sm :rounded [:py 2])
           :style {:display "block"
                   :overflow-x "overlay"}}
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
                           docname (get row :xt/id)]

                       ;; TODO think about saving symbols in xtdb
                       (cond (= :xt/id h)
                             (methods/rendercontent ztx ctx {:data (symbol v) :key h :ann {:zd/content-type :edn}})

                             (and (some? v) docname)
                             (let [{{anns :ann} :zd/meta} (loader/get-doc ztx (symbol docname))]
                               (methods/renderkey ztx {} {:key h :data (get row h) :ann (get anns h)}))

                             (some? v)
                             (methods/rendercontent ztx ctx {:data v :key h :ann {:zd/content-type :edn}})))]))]))
        (into [:tbody]))])
