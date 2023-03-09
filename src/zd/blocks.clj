(ns zd.blocks
  (:require
   [zd.zentext :as zentext]
   [clojure.pprint :as pprint]
   [zd.impl]
   [stylo.core :refer [c]]
   [clojure.string :as str]
   [zd.methods :as methods]))

(defn keystr [key]
  (->> key
       (map name)
       (clojure.string/join ".")
       (str ".")))

(defmethod methods/renderkey :country [ztx ctx block]

  )

(defmethod methods/renderkey :zd/invalid-links
  [ztx ctx {:keys [data] :as block}]
  [:div {:class (c  [:py 2] [:px 0])}
   [:span {:class (c [:text :gray-600] {:font-weight 400})}
    "Broken links"]
   (for [[path links] (group-by :path data)]
     [:div {:class (c [:py 2] :text-sm)}
      [:div {:class (c [:text :gray-600] :border-b {:font-weight "400"})}
       (keystr path)]
      (for [{doc-symbol :to} links]
        [:div {:class (c [:py 0.5] {:margin-right "0.5rem"})}
         (zd.impl/symbol-link ztx doc-symbol)])])])

(defmethod methods/renderkey :title
  [ztx {doc :doc} {title :data :as block}]
  [:h1 {:class (c :flex :items-center)}
   (if-let [img (or (:avatar doc) (:logo doc))]
     [:img {:src img
            :class (c [:w 8] [:h 8] :inline-block [:mr 2] {:border-radius "100%"})}]
     (when-let [icon (:icon doc)]
       [:i {:class (str (str/join " " (map name icon))
                        " "
                        (name (c [:mr 2] [:text :gray-600])))}]))
   title])

(defmethod methods/renderkey :zd/backlinks
  [ztx ctx block]
  [:div {:class (c [:text :gray-600])}
   [:div {:class (c  [:py 2] [:px 0])}
    [:span {:class (c [:text :gray-600] {:font-weight 400})}
     "Linked to"]
    (for [[path links] (group-by :path (:data block))]
      [:div {:class (c [:py 2] :text-sm)}
       [:div {:class (c [:text :gray-600] :border-b [:mb 2] {:font-weight "400"})}
        (keystr path)]
       [:div
        (for [{doc-symbol :doc} links]
         [:div {:class (c [:py 0.5])} (zd.impl/symbol-link ztx doc-symbol)])]])]])

(defmethod methods/rendercontent :edn
  [ztx ctx {data :data}]
  [:div.code-block
   [:pre {:class (c :text-sm) :style {:white-space "pre-wrap"}}
    [:i.fas.fa-clipboard-list.copy-button
     {:title "Click to Copy"
      :style {:position  "relative"
              :float     "right"
              :top       "5px"
              :right     "20px"}}]
    [:code {:style {:word-wrap "break-word"} :class (str "language-edn hljs")}
     (if (string? data)
       data
       (with-out-str (pprint/pprint data)))]]])

(defmethod methods/rendercontent :zentext
  [ztx ctx {:keys [data] :as block}]
  [:div {:class (c [:px 0] [:py 1] [:bg :white] {:word-wrap "break"})}
   (zentext/parse-block ztx data block)])
