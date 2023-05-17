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
    (or (set? data) (and (vector? data)
                         (every? symbol? data)))
    (->> data
         (mapv (fn [x] (methods/rendercontent ztx ctx (assoc block :data x))))
         (into [:div {:class (c :flex [:space-x 3] {:flex-wrap "wrap"})}
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
      [:code {:style {:background "white"
                      :border :none
                      :word-wrap "break-word"}
              :class (str "language-edn hljs")}
       (if (string? data)
         data
         (with-out-str (pprint/pprint data)))]]]))

(defmethod methods/rendercontent :zentext
  [ztx ctx {:keys [data] :as block}]
  [:div {:class (c [:py 1])}
   (zentext/parse-block ztx data block)])

(defmethod methods/rendercontent :datalog
  [ztx ctx {{headers :table-of} :ann data :data :as block}]
  (let [result
        (if (vector? data)
          (apply d/query (concat [ztx] data))
          (d/query ztx data))]
    (if (set? result)
      (if (seq headers)
        (comp/table ztx ctx headers (map first result))
        (let [headers* (->> (map first result)
                            (mapcat keys)
                            (set)
                            (sort-by name))]
          (comp/table ztx ctx headers* (map first result))))
      (methods/rendercontent ztx ctx {:data result
                                      :k (:key block)
                                      :ann {:zd/content-type :edn}}))))
