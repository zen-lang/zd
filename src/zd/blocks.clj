(ns zd.blocks
  (:require
   [zd.link :as link]
   [zd.zentext :as zentext]
   [clojure.pprint :as pprint]
   [stylo.core :refer [c]]
   [clojure.string :as str]
   [zd.methods :as methods]))

(defn keystr [key]
  (->> key
       (map name)
       (clojure.string/join ".")
       (str ".")))

(defmethod methods/renderkey :linkedin
  [ztx {{m :zd/meta} :doc} {data :data :as block}]
  (let [id (if (and data (not (str/blank? data)))
             data
             (last (str/split (str (:docname m)) #"\.")))]
    [:a {:class (str "badge " (name (c :border [:m 1]  :inline-flex :rounded
                                       [:py 1]
                                       :text-sm [:text :blue-500] [:px 2])))
         :href
         (if (str/starts-with? (str (:docname m)) "organizations.")
           (str "https://www.linkedin.com/company/" id "/")
           (str "https://www.linkedin.com/in/" id "/"))}
     [:i.fa-brands.fa-linkedin]]))

(defmethod methods/renderkey :zd/docname
  [ztx ctx {data :data :as block}]
  [:div {:class (str "badge " (name (c :border [:my 1] [:mr 2]  :inline-flex :rounded [:p 0])))}
   [:div {:class (c :inline-block [:px 2] [:bg :gray-100] [:py 0.5] :text-sm [:text :gray-700]
                    {:font-weight "400" :padding-top ".3rem"})}
    [:i.fas.fa-file]]
   [:div {:class (c [:px 2] [:py 0.5] :inline-block :text-sm)}
    data]])

(defmethod methods/renderkey :link-badge
  [ztx ctx {data :data k :key}]
  [:div {:class (c :border [:m 1]  :inline-flex :rounded [:p 0])}
   [:a {:href data
        :target "_blank"
        :class (c :inline-block [:px 2] [:bg :gray-100] [:py 0.5] :text-sm [:text :blue-600] {:font-weight "400"}
                  [:hover [:bg :gray-200]])}
    k]])

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
         (link/symbol-link ztx doc-symbol)])])])

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
         [:div {:class (c [:py 0.5])} (link/symbol-link ztx doc-symbol)])]])]])

(defmethod methods/rendercontent :edn
  [ztx ctx {:keys [data] :as block}]
  (cond
    (or (string? data) (number? data)) (zd.zentext/parse-block ztx (str data) block)
    (symbol? data) (link/symbol-link ztx data)
    (keyword? data) (zd.zentext/parse-block ztx (str data) block)
    (set? data)
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
      [:code {:style {:word-wrap "break-word"} :class (str "language-edn hljs")}
       (if (string? data)
         data
         (with-out-str (pprint/pprint data)))]]]))

(defmethod methods/rendercontent :zentext
  [ztx ctx {:keys [data] :as block}]
  [:div {:class (c [:px 0] [:py 1] [:bg :white] {:word-wrap "break"})}
   (zentext/parse-block ztx data block)])

(defmethod methods/renderkey :badge
  [ztx ctx {key :key :as block}]
  [:div {:class (str "badge " (name (c :border [:my 1] [:mr 2]  :inline-flex :rounded [:p 0])))}
   [:div {:class (c :inline-block [:px 2] [:bg :gray-100] [:py 0.5] :text-sm [:text :gray-700] {:font-weight "400"})}
    key]
   [:div {:class (c [:px 2] [:py 0.5] :inline-block :text-sm)}
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

(defmethod methods/inline-method :symbol-link
  [ztx m s ctx]
  (link/symbol-link ztx s))

(defmethod methods/inline-method :code
  [ztx m s ctx]
  [:code {:class (c [:px 1.5] [:py 1] [:bg :gray-200]
                    :text-sm
                    {:border-radius "4px"
                     :font-family "ui-monospace,SFMono-Regular,SF Mono,Menlo,Consolas,Liberation Mono,monospace"})}
   s])

(defmethod methods/inline-method :b
  [ztx m s ctx]
  [:b s])

(defn img-src [ctx src]
  (when src
    (cond (str/starts-with? src "http") src
          (str/starts-with? src "/") src
          :else (str "/" (get-in ctx  [:block :page :name]) "/" src))))

(defmethod methods/inline-method
  :img
  [ztx m arg ctx]
  (let [[src alt] (str/split arg #"\s+" 2)
        src (img-src ctx src)]
    [:img {:src src :alt alt}]))

(defmethod methods/inline-function
  :img
  [ztx m args ctx]
  (let [[src opts & _]  args
        src (img-src ctx src)]
    [:img (assoc opts :src src)]))

(defmethod methods/inline-function
  :echo
  [ztx m args ctx]
  [:span "((" m (pr-str args) "))"])

;; TODO fix this zentext function
#_(defmethod methods/inline-function
  :resource
  [ztx m [sym & path] ctx]
  (if-let [sym (zd.db/get-resource ztx sym)]
    (get-in sym path)
    [:div "Could not find " (pr-str sym)]))

(defmethod methods/inline-method :a
  [ztx m arg ctx]
  (let [[src text] (str/split arg #"\s+" 2)]
    [:a {:href src :class (c [:text :blue-700] [:hover [:underline]])} " " (or text src)]))

(defmethod methods/inline-method
  :src
  [ztx m arg ctx]
  [:a {:class (c [:text :green-600] :title "TODO")}
   (str arg)])

(defmethod methods/inline-method :default
  [ztx m arg ctx]
  [:span {:class (c [:text :red-600] [:bg :red-100])} (str "No inline-method for " m " arg:" arg)])

(defmethod methods/process-block "code" [ztx _ lang cnt]
  [:div.code-block
   [:pre {:class (c :text-sm)
          :style {:position "relative" :white-space "pre-wrap"}}
    [:code {:style {:word-wrap "break-word"} :class (str "language-" lang " hljs")} cnt]]])

(defmethod methods/process-block :default [ztx tp args cnt]
  [:pre {:params args :tp tp}
   [:code.hljs cnt]])

(defmethod methods/process-block "table" [ztx _ _ args]
  (let [[cols & rows] (->> (str/split-lines args) (mapv (fn [x] (str/split x #"\|"))))]
    [:table {:class (c :shadow-sm :rounded)}
     [:thead
      (into [:tr] (->> cols (mapv (fn [k] [:th {:class (c [:px 4] [:py 2] :border [:bg :gray-100])} k]))))]
     (into [:tbody]
           (->> rows
                (mapv (fn [x]
                        (into [:tr]
                              (->> x (mapv (fn [v] [:td {:class (c [:px 4] [:py 2] :border)} v]))))))))]))

