(ns zd.blocks.content
  (:require
   [zd.components :as comp]
   [zd.datalog :as d]
   [zd.zentext :as zentext]
   [zd.link :as link]
   [clojure.pprint :as pprint]
   [stylo.core :refer [c]]
   [zd.methods :as methods]
   [zd.utils :as utils]))

(defmethod methods/rendercontent :edn
  [ztx ctx {:keys [data] :as block}]
  (cond
    (or (string? data) (number? data)) (zentext/parse-block ztx (str data) block)
    (symbol? data) (link/symbol-link ztx data)
    (keyword? data) (zentext/parse-block ztx (str data) block)
    (or (set? data) (vector? data))
    (->> data
         (mapv (fn [x] (methods/rendercontent ztx ctx (assoc block :data x))))
         (into [:div {:class (c :flex [:space-x 1.5] {:flex-wrap "wrap"})}
                [:div {:class (c [:text :gray-500] :text-sm)} "#"]]))
    :else
    [:div.code-block
     [:pre {:class (c :text-sm) :style {:white-space "pre-wrap"}}
      [:i.fas.fa-clipboard-list.copy-button
       {:title "Click to Copy"
        :style {:position  "relative"
                :float     "right"
                :top       "5px"
                :right     "20px"}}]
      ;; TODO fix edn display code block
      [:code {:style {:background "white !important"
                      :word-wrap "break-word"}}
       [:span {:class (str (str "language-edn hljs ")  (c [:bg "white"]))}
        (if (string? data)
          data
          (with-out-str (pprint/pprint data)))]]]]))

(defmethod methods/rendercontent :zentext
  [ztx ctx {:keys [data] :as block}]
  [:div (zentext/parse-block ztx data block)])

(defmethod methods/rendercontent :datalog
  ;; TODO rename table-of to :table for consistency
  [ztx ctx {{headers :table-of} :ann data :data :as block}]
  (let [result (if-let [params (:in data)]
                 (apply d/query ztx data params)
                 (d/query ztx data))]
    (if (and (seq result)
             ;; TODO fix this check
             (map? (ffirst result))
             (seq headers))
      (comp/table ztx ctx headers (map first result))
      [:span (pr-str result)])))
