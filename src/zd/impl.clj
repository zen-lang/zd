(ns zd.impl
  (:require
   [clojure.string :as str]
   [stylo.core :refer [c]]
   [zd.db]
   [zd.zentext]
   [zd.methods :refer [annotation inline-method render-block render-content render-key process-block]]))

(defmethod annotation
  :default
  [nm params]
  (println ::missed-annotation nm)
  {:errors {nm {:params params
                :message (str "No rule for " nm)}}})

(defmethod annotation :view-only
  [nm params]
  {:view-only true})

(defmethod annotation :badge
  [nm params]
  {:block :badge
   :badge params})

(defmethod annotation :attribute
  [nm params]
  {:block :attribute
   :attribute params})

(defmethod annotation :href
  [nm params]
  {:content :href})

(defmethod annotation :img
  [nm params]
  {:content :img})

(defmethod annotation :title
  [nm params]
  {:title params})

(defmethod annotation :hide
  [nm params]
  {:block :none})

(defmethod annotation :table
  [nm params]
  {:block :table
   :table params})

(defmethod inline-method :symbol-link
  [ztx m s]
  (if-let [res (zd.db/get-resource ztx (symbol s))]
    [:a {:href (str "/" s) :class (c [:text :blue-600])} s]
    [:a {:href (str "/" s) :class (c [:text :red-600] [:bg :red-100]) :title "Broken Link"} s]))


(defmethod inline-method
  :img
  [ztx m arg]
  (let [[src alt] (str/split arg #"\s+" 2)]
    [:img {:src src :alt alt}]))

(defmethod inline-method :a
  [ztx m arg]
  (let [[src text] (str/split arg #"\s+" 2)]
    [:a {:href src :class (c [:text :blue-700])} (or src text)]))

(defmethod inline-method
  :src
  [ztx m arg]
  [:a {:class (c [:text :green-600] :title "TODO")}
   (str arg)])

(defmethod inline-method
  :default
  [ztx m arg]
  [:span {:class (c [:text :red-600] [:bg :red-100])} (str "No inline-method for " m " arg:" arg)])

(defmethod process-block "code" [ztx _ lang cnt]
  [:pre [:code {:class (str "language-" lang " hljs")} cnt]])

(defmethod process-block :default [ztx tp args cnt]
  [:pre {:params args :tp tp}
   [:code.hljs cnt]])

(defmethod render-key :default [_ & _] nil)

(defmethod render-key
  [:title]
  [_ {title :data}]
  [:h1 {:class (c [:mb 0] :border-b)} title])

(defmethod render-key
  [:summary]
  [ztx block]
  [:div {:class (c [:text :gray-600])}
   (render-content ztx block)])

(defmethod render-content
  :md
  [ztx {data :data}]
  [:div {:class (c [:px 0] [:py 2] [:bg :white])}
   (zd.zentext/parse-block ztx data)])

(defmethod render-content
  :default
  [ztx {data :data}]
  (cond
    (string? data) [:span data]
    (keyword? data) [:span {:class (c [:text :green-600])} (str data)]
    ;; TODO: check link
    (symbol? data) [:a {:href (str "/" data) :class (c [:text :blue-600])}
                    (if-let [res (zd.db/get-resource ztx data)]
                      (or (:title res) data)
                      (str  data))]
    (set? data) (conj (into [:div {:class (c :flex [:space-x 4])}
                             [:div {:class (c [:text :gray-500])} "#{"]]
                            (mapv (fn [x] (render-content ztx {:data x}))data))
                      [:div {:class (c [:text :gray-500])} "}"])

    (list? data)
    [:pre [:clode {:class (str "language-clojure hljs")} (pr-str data)]]

    (sequential? data)
    (conj (into [:ul {:class (c)}]
                (->> data
                     (mapv (fn [x] [:li (render-content ztx {:data x})])))))

    (map? data)
    (conj (into [:ul {:class (c)}]
                (->> data
                     (mapv (fn [[k v]]
                             [:li [:b (str k)] " "
                              (render-content ztx {:data v})])))))

    :else [:pre (pr-str data)]))

(defn capitalize [k]
  (let [s (if (keyword? k) (subs (str k) 1) (str k))]
    (str/capitalize (str/replace s #"-" " "))))


(defn keypath [path title]
  (let [id (str/join path)]
    [:a {:id id :href (str "#" id)} (or title id)]))

(defmethod render-block
  :default
  [ztx {ann :annotations data :data path :path :as block}]
  (when-let [ann (:block ann)] (println :missed-render-block ann))
  [:div {:class (c [:py 2])}
   [(keyword (str "h" (inc (count path))))
    (keypath path (or (:title ann) (let [k (last path)] (capitalize k))))]
   (render-content ztx block)])

(defmethod render-block :none [ztx block])

(defmethod render-block
  :badge
  [ztx {data :data path :path :as block}]
  [:div {:class (c :border [:m 1]  :inline-flex :rounded [:p 0])}
   [:div {:class (c :inline-block [:px 2] [:bg :gray-200] [:py 0.5] :text-sm [:text :gray-700] {:font-weight "400"})}
    (subs (str (last path)) 1) ]
   [:div {:class (c [:px 2] [:py 0.5] :inline-block)}
    (render-content ztx block)]])

(defmethod render-block
  :attribute
  [ztx {data :data path :path :as block}]
  [:div {:title "attribute" :class (c [:py 0.5] :flex :border-b :items-baseline [:space-x 4])}
   [:div {:class (c  [:text :gray-600] {:font-weight "500"})}
    (subs (str (last path)) 1) ]
   [:div {:class (c )}
    (render-content ztx block)]])
