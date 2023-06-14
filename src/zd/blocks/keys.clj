(ns zd.blocks.keys
  (:require
   [zd.components :as comp]
   [clojure.string :as str]
   [stylo.core :refer [c]]
   [zd.methods :as methods]))

;; TODO use link badge for linkedin prop?
(defmethod methods/renderkey :linkedin
  [ztx {{m :zd/meta} :doc} {data :data :as block}]
  (let [id (if (and data (not (str/blank? data)))
             data
             (last (str/split (str (:docname m)) #"\.")))]
    [:a {:class (str "badge " (name (c :border [:m 1]  :inline-flex :rounded
                                       [:py 1]
                                       :text-sm [:text :blue-500] [:px 2])))
         :id :linkedin
         :href
         (if (str/starts-with? (str (:docname m)) "organizations.")
           (str "https://www.linkedin.com/company/" id "/")
           (str "https://www.linkedin.com/in/" id "/"))}
     [:i.fa-brands.fa-linkedin]]))

(defmethod methods/renderkey :link-badge
  [ztx ctx {data :data k :key}]
  [:div {:class (c :border [:m 1]  :inline-flex :rounded [:p 0])}
   [:a {:href data
        :target "_blank"
        :class (c :inline-block [:px 2] [:bg :gray-100] [:py 0.5] :text-sm [:text :blue-600] {:font-weight "400"}
                  [:hover [:bg :gray-200]])}
    k]])

(defmethod methods/renderkey :title
  [ztx {doc :doc} {title :data :as block}]
  [:h1 {:class (c :flex :items-center [:m 0] [:py 4]) :id "title"}
   (if-let [img (or (:avatar doc) (:logo doc))]
     [:img {:src img
            :class (c [:w 8] [:h 8] :inline-block [:mr 2] {:border-radius "100%"})}]
     (when-let [icon (:icon doc)]
       [:i {:class (str (str/join " " (map name icon))
                        " "
                        (name (c [:mr 2] [:text :gray-600])))}]))
   title])

(defmethod methods/renderkey :badge
  [ztx ctx {key :key :as block}]
  [:div {:class (c :border [:my 1] [:mr 2] :inline-flex :items-baseline :rounded)}
   [:div {:class (c :inline-block [:px 1] [:bg :gray-100] [:py 0.5] :text-sm [:text :gray-700] {:font-weight "400"})}
    key]
   [:div {:class (c [:px 1] [:py 0.5] :inline-block :text-sm)}
    (methods/rendercontent ztx ctx block)]])

(defmethod methods/renderkey :attribute
  [ztx ctx {k :key :as block}]
  [:div {:title "attribute" :class (c [:py 0.5] :flex :items-center [:space-x 4])}
   [:div {:class (c  [:text :gray-600] {:font-weight "500"})}
    k]
   [:div {:class (c)}
    (methods/rendercontent ztx ctx block)]])

(defmethod methods/renderkey :none
  [ztx ctx block])

(defmethod methods/renderkey :hide
  [ztx ctx block])

(defmethod methods/renderkey :table
  [ztx ctx {{headers :table} :annotations data :data}]
  (if (and (sequential? data) (every? map? data))
    (comp/table ztx ctx (or headers (keys (first data))) data)
    [:pre (pr-str data)]))
