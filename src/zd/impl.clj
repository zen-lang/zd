(ns zd.impl
  (:require
   [clojure.string :as str]
   [stylo.core :refer [c]]
   [zd.db]
   [zd.zentext]
   [sci.core]
   [clj-yaml.core]
   [zd.methods :refer [annotation inline-method inline-function render-block render-content render-key process-block]]))

(defmethod annotation :default
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

(defmethod annotation :attr
  [nm params]
  {:block :attribute
   :attribute params})

(defmethod annotation :href
  [nm params]
  {:content :href})

(defmethod annotation :link
  [nm params]
  {:content :link})

(defmethod annotation :img
  [nm params]
  {:content :img
   :img params})

(defmethod annotation :title
  [nm params]
  {:title params})

(defmethod annotation :hide
  [nm params]
  {:block :none})

(defmethod annotation :table
  [nm params]
  {:content :table
   :table params})

(defmethod annotation :hiccup
  [nm params]
  {:content :huccup
   :huccup params})

(defmethod annotation :edn
  [nm params]
  {:content :edn})

(defmethod annotation :yaml
  [nm params]
  {:content :yaml})

(defn symbol-link [ztx s]
  (if-let [res (zd.db/get-resource ztx (symbol s))]
    [:a {:href (str "/" s) :class (c [:text :blue-600])}
     (when-let [img (or (get-in res [:avatar]) (get-in res [:logo]))]
       [:img {:src img :class (c [:h 4] :inline-block [:mr 1] {:border-radius "100%" :margin-bottom "1px"})}])
     (or (:title res) s)]
    [:a {:href (str "/" s) :class (c [:text :red-600] [:bg :red-100]) :title "Broken Link"} s]))

(defmethod inline-method :symbol-link
  [ztx m s]
  (symbol-link ztx s))


(defmethod inline-method
  :img
  [ztx m arg]
  (let [[src alt] (str/split arg #"\s+" 2)]
    [:img {:src src :alt alt}]))

(defmethod inline-method :a
  [ztx m arg]
  (let [[src text] (str/split arg #"\s+" 2)]
    [:a {:href src :class (c [:text :blue-700])} " " (or text src)]))

(defmethod inline-method
  :src
  [ztx m arg]
  [:a {:class (c [:text :green-600] :title "TODO")}
   (str arg)])

(defmethod inline-method :default
  [ztx m arg]
  [:span {:class (c [:text :red-600] [:bg :red-100])} (str "No inline-method for " m " arg:" arg)])

(defmethod process-block "code" [ztx _ lang cnt]
  [:pre {:class (c :text-sm)}
   [:code {:class (str "language-" lang " hljs")} cnt]])

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

(defmethod render-content :md
  [ztx {data :data}]
  [:div {:class (c [:px 0] [:py 2] [:bg :white])}
   (zd.zentext/parse-block ztx data)])

(defmethod render-content :img
  [ztx {page :page {img :img} :annotations data :data}]
  (let [path (str/join "/" (butlast (str/split (:zd/file page) #"/" )))
        src (cond
              (str/starts-with? data "http") data
              (str/starts-with? data "/") data
              :else (str path "/" data))]
    [:img (merge img {:src src})]))

(defmethod render-content :default
  [ztx {data :data :as block}]
  (cond
    (string? data) [:span data]
    (or (keyword? data) (boolean? data))
    [:span {:class (c [:text :green-600])} (str data)]
    ;; TODO: check link
    (nil? data) ""
    (symbol? data) (symbol-link ztx data)
    (number? data) (str data)
    (set? data) (conj (into [:div {:class (c :flex [:space-x 4])}
                             [:div {:class (c [:text :gray-500])} "#{"]]
                            (mapv (fn [x] (render-content ztx {:data x}))data))
                      [:div {:class (c [:text :gray-500])} "}"])

    (list? data)
    [:pre [:clode {:class (str "language-clojure hljs")} (pr-str data)]]

    (sequential? data)
    (if (keyword? (first data))
      (render-content ztx (assoc-in block [:annotations :content] :hiccup))
      (conj (into [:ul {:class (c)}]
                  (->> data
                       (mapv (fn [x] [:li (render-content ztx {:data x})]))))))

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
  [:div {:class (c [:py 2])}
   (when-let [ann (:block ann)]
     (println :missed-render-block ann)
     [:div {:class (c [:text :red-800])}
      (str "Missed render-block for " ann)])
   [(keyword (str "h" (inc (count path))))
    (keypath path (or (:title ann) (let [k (last path)] (capitalize k))))]
   (render-content ztx block)])

(defmethod render-block :none
  [ztx block])

(defmethod render-block :badge
  [ztx {data :data path :path :as block}]
  [:div {:class (c :border [:m 1]  :inline-flex :rounded [:p 0])}
   [:div {:class (c :inline-block [:px 2] [:bg :gray-200] [:py 0.5] :text-sm [:text :gray-700] {:font-weight "400"})}
    (subs (str (last path)) 1) ]
   [:div {:class (c [:px 2] [:py 0.5] :inline-block)}
    (render-content ztx block)]])

(defmethod render-block :attribute
  [ztx {data :data path :path :as block}]
  [:div {:title "attribute" :class (c [:py 0.5] :flex :border-b :items-baseline [:space-x 4])}
   [:div {:class (c  [:text :gray-600] {:font-weight "500"})}
    (subs (str (last path)) 1) ]
   [:div {:class (c )}
    (render-content ztx block)]])

(defn table [ztx cfg data]
  (if-let [headers (or (:columns cfg)
                       (and (sequential? data) (map? (first data))
                            (keys (first data))))]
    [:table {:class (c :shadow-sm :rounded)}
     [:thead
      (into [:tr] (->> headers (mapv (fn [k] [:th {:class (c [:px 4] [:py 2] :border [:bg :gray-100])}
                                              (capitalize k)]))))]
     (into [:tbody]
           (->> data
                (mapv (fn [x]
                        (into [:tr]
                              (->> headers
                                   (mapv (fn [k]
                                           [:td
                                            {:class (c [:px 4] [:py 2] :border)}
                                            (render-content ztx {:data (get x k)})]))))))))]
    [:pre (pr-str data)]))

(defmethod render-content :table
  [ztx {ann :annotations data :data path :path :as block}]
  (table ztx (or (:table ann) {}) data))


(defmethod render-content :yaml
  [ztx {ann :annotations data :data path :path :as block}]
  [:pre {:class (c :text-sm)}
   [:code {:class (str "language-yaml hljs")} (clj-yaml.core/generate-string data)]])

(defmethod render-block :zen/errors
  [ztx {ann :annotations errors :data path :path :as block}]
  (when (seq errors)
    [:div {:class (c [:text :red-700] [:py 2] [:px 4])}
     [:ul {:class (c :font-bold :text-lg [:mb 2] :border-b)} "Errors"]
     (for [err (sort-by :type errors)]
       [:li {:class (c [:mb 1] :flex [:space-x 3])}
        [:span {:class (c [:text :green-600])} (str (:path err))]
        [:span {:class (c)} (:message err)]])]))


(def c-macro ^:sci/macro
  (fn [_&form _&env & rules]
    #_(apply stylo.core/c' rules)))

(defmethod render-content :link
  [ztx {ann :annotations data :data path :path :as block}]
  [:a {:href data :class (c [:text :blue-600])} data])

(defmethod render-content :hiccup
  [ztx {ann :annotations data :data path :path :as block}]
  (let [ctx (sci.core/init {:bindings {'search (fn [filter] (zd.db/search ztx filter))
                                       'select (fn [fltr] (zd.db/select ztx fltr))
                                       'c  c-macro
                                       'table  (fn [data opts] (table ztx (or opts {}) data))}})
        res (try (sci.core/eval-form ctx data)
                 (catch Exception e
                   [:div {:class (c [:text :red-600])}
                    [:pre (pr-str data)]
                    (pr-str e)]))]
    (if (and (vector? res) (keyword? (first res)))
      res
      [:pre (pr-str res)])))

(defmethod inline-function
  :echo
  [ztx m args]
  [:span "((" m (pr-str args) "))"])
