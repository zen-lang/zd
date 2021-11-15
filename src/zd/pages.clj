(ns zd.pages
  (:require
   [zen.core :as zen]
   [zd.parser]
   [zd.markdown]
   [hiccup.core :as hiccup]
   [hiccup.util]
   [markdown.core]
   [clojure.string :as str]
   [stylo.core :refer [c]]
   [clojure.string :as str]))

(defn to-html [x] (hiccup/html x))

(defn layout [ztx content]
  [:html
   [:head
    [:style (stylo.core/compile-styles @stylo.core/styles)]
    [:meta {:charset "UTF-8"}]
    [:link {:rel"stylesheet" :href "/static/gh.css"}]
    [:link {:href "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/styles/default.min.css", :rel "stylesheet"}]
    [:script {:src "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/highlight.min.js"}]
    [:script {:src "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/languages/clojure.min.js"}]
    [:script "hljs.highlightAll()"]]
   [:body {:class (c [:bg :gray-100])}
    content
    ]])

;; (instance? java.util.Date. (java.util.Date.))


(defn render-value [ztx k v]
  (cond

    (and (map? v) (:format v))
    (case (:format v)
      "md"   [:div {:class (c [:px 0] [:py 4] [:bg :white])}
              (zd.markdown/parse ztx (:content v))
              #_[:div.markdown-body
               (hiccup.util/as-str (markdown.core/md-to-html-string (:content v)))]]
      [:div "Unknown format " (:format v)
       [:pre (:content v)]])

    (and (vector? v) (= :div (first v)))
    [:div.markdown-body (eval v)]

    (keyword? v)
    [:span {:class (c [:text :green-600])} v]

    (symbol? v)
    [:a {:href (str v) :class (cond (zd.parser/get-doc ztx v) (c [:text :blue-500])
                                    (zen.core/get-symbol ztx v) (c [:text :green-500])
                                    :else (c [:text :red-500]))} (str v)]

    (string? v)
    (if (str/starts-with? v "http")
      [:a {:href v :class (c [:text :blue-500])} v]
      [:div v])

    (set? v)
    (->>
      (for [x v]
        (render-value ztx k x))
      (into [:div {:class (c :flex [:space-x 2])} [:div {:class (c [:text :gray-500])} "#{"]]))

    :else
    [:div (pr-str v)]))

(defn build-tree [ztx doc]
  (->>
   (sort-by first (:zd/resources @ztx))
   (reduce (fn [acc [nm doc]]
             (let [parts (interpose :items (str/split (name nm) #"\."))]
               (assoc-in acc parts {:title (or (:zd/title doc) nm)
                                    :href (str nm)
                                    :errors (when-let [err (:zen/errors doc)]
                                              (count err))})))
           {})))

(defn render-items [item & [k]]
  [:div
   (if-let [h  (:href item)]
     [:a {:href h :class (c [:text :blue-500])} (:title item) (when-let [e (:errors item)]
                                                                [:span {:class (c [:text :red-500] :text-xs [:px 1])} e])]
     [:div k])
   (into [:div {:class (c [:pl 4])}]
         (for [[k it] (->> (:items item)
                           (sort-by :title))]
           (render-items it k)))])

(defn navigation [ztx doc]
  [:div {:class (c [:px 4] [:w 80] [:text :gray-600]  :text-sm)}
   (into [:div]
         (for [[k it] (->> (build-tree ztx doc)
                           (sort-by :title))]
           (render-items it k)))])

(defmulti render-key (fn [ztx k v] k))


(defmethod render-key [:person/avatar]
  [ztx k v]
  [:div {:class (c  [:p 2])}
   [:img {:src v :class (c [:w 20] :rounded :box-shadow)}]])

(defmethod render-key [:zen/errors]
  [ztx k v]
  [:div {:class (c [:bg :red-100]  [:p 2])}
   [:h3 {:class (c [:text :red-700] [:my 1] :text-xl)} "zen/errors"]
   [:div.markdown-body
    [:ul
     (for [e v]
       [:li {:class (c [:text :red-500] :text-xs)}
        (:message e)
        "  @ "
        (pr-str (:path e))
        (when (:schema e)
          (str  " by "        (pr-str (:schema e))))])]]])

(defmethod render-key :default
  [ztx k v]
  (if (:format v)
    [:div  {:class (c {:border-bottom "1px solid #eaecef"})}
     [:div {:class (c [:text :gray-600] :text-sm {:font-weight "400"})}
      (str/join "~" k)]
     (render-value ztx k v)]
    [:div {:class (c :flex [:space-x 2] [:pt 1] [:pb 0.5]
                     :items-center
                     {:border-bottom "1px solid #eaecef"})}
     [:div {:class (c [:text :gray-600] :text-sm [:w 40] {:font-weight "400"})}
      (str/join "~" k)]
     (render-value ztx k v)]))

(defn page [ztx doc]
  [:div {:class (c [:w 260] [:bg :white] [:py 4] [:px 8] :shadow-md)}
    (when-let [t (:zd/title doc)]
      [:div {:class (c :flex :border-b [:pt 2] [:pb 1])} 
       [:div {:class (c :flex-1 [:text :gray-800] :font-bold  {:font-size "1.5rem"})} t]
       [:div (render-value ztx :zd/tags (:zd/tags doc))]])
    (->>
     (for [k (:zd/keys doc)]
       (when-not (contains? #{:zd/title :zd/tags} k)
         (let [v (get-in doc k)]
           (render-key ztx k v))))
      (into [:div {:class (c )}]))])

(defn links [ztx doc]
  [:div {:class (c [:px 4]  [:text :gray-600])}
   (->>
    (for [[pth links] (zd.parser/get-links ztx (:zd/name doc))]
      [:div
       [:div  {:class (c :font-bold :text-xs :border-b)} (pr-str pth)]
       (into
        [:div {:class (c [:pl 0])}]
        (for [[from opts] links]
          [:a {:href (str from) :class (c [:text :blue-500] :block)}
           from]))])
    (into [:div]))])


(defn generate-page [ztx doc]
  [:div {:class (c [:p 4] :flex [:space-x 4])}
   (navigation ztx doc)
   (page ztx doc)
   (links ztx doc)])

(defn zen-page [ztx doc]
  [:div {:class (c [:w 260] [:bg :white] [:py 4] [:px 8] :shadow-md)}
   (when-let [t (:zen/name doc)]
     [:div {:class (c :flex :border-b [:pt 2] [:pb 1])} 
      [:div {:class (c :flex-1 [:text :gray-800] :font-bold  {:font-size "1.5rem"})} t]])
   [:pre (pr-str doc)]])

(defn generate-zen-page [ztx doc]
  [:div {:class (c [:p 4] :flex [:space-x 4])}
   (navigation ztx doc)
   (zen-page ztx doc)])

(defn render-page [ztx doc]
  (->> (generate-page ztx doc)
       (layout ztx)
       (to-html)))

(defn render-zen [ztx doc]
  (->> (generate-zen-page ztx doc)
       (layout ztx)
       (to-html)))

(comment

  (def ztx (zen/new-context {:zd/path "zd"}))
  (def doc )

  (render-page ztx 'zd.features.format "/tmp/format.html")
  (render-page ztx 'zd "/tmp/zd.html")

  )
