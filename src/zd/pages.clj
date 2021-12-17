(ns zd.pages
  (:require
   [zen.core :as zen]
   [zd.zentext]
   [zd.db]
   [hiccup.core :as hiccup]
   [hiccup.page]
   [hiccup.util]
   [markdown.core]
   [clojure.string :as str]
   [stylo.core :refer [c c?]]
   [garden.core]
   [stylo.rule  :refer [join-rules]]
   [clojure.string :as str]))

(defn to-html [x] (hiccup/html x))


(def closed-node-style (c [:bg :red-500]))

(defn c* [& args]
  (join-rules args))

(def common-style
  [:body {:font-family "sohne, \"Helvetica Neue\", Helvetica, Arial, sans-serif;"}
   [:h1 {:font-size "46px" :font-weight "700" :border-bottom "1px solid #f1f1f1"}]
   [:h2 {:font-size "32px" :font-weight "700" :line-height "40px" :border-bottom "1px solid #f1f1f1"}]
   [:h3 {:font-size "24px" :font-weight "700" :line-height "36px" :border-bottom "1px solid #f1f1f1"}]
   [:ul {:list-style "inside"
         :line-height "24px"}
    [:li {:display "list-item"}]
    [:ul {:margin-left "2rem"}]]
   [:ol {:list-style "disk inside"
         :line-height "24px"}
    [:li {:display "list-item"}]
    [:ol {:margin-left "2rem"}]]
   [:p {:margin-bottom "1rem"}]
   [:.hljs (c* [:bg :gray-100] :shadow-sm
               :border)]
   [:pre {:margin-top "1rem" :margin-bottom "1rem"}]
   [:.closed {:display "none"}]
   [:.pl-4  {:padding-left "1rem"}]
   [:.toggler {:padding-left "4px"
               :padding-right "4px"
               :padding-top "2px"
               :padding-bottom "2px"}]
   [:.rotateToggler {:transform "rotate(-90deg)"}]])


(defn layout [ztx content]
  [:html
   [:head
    [:style (garden.core/css common-style)]
    [:style (stylo.core/compile-styles @stylo.core/styles)]
    [:meta {:charset "UTF-8"}]
    [:link {:href "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/styles/default.min.css", :rel "stylesheet"}]
    [:script {:src "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/highlight.min.js"}]
    [:script {:src "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/languages/clojure.min.js"}]
    [:script {:src "https://kit.fontawesome.com/c38313ee57.js" :crossorigin "anonymous"}]
    [:script "hljs.highlightAll()"]]
   [:body {:class (c [:bg :gray-100])}
    content
    [:script (slurp "./src/js/tree.js")]]])

;; (instance? java.util.Date. (java.util.Date.))
(defn render-value [ztx k v]
  (cond

    (and (map? v) (:format v))
    (case (:format v)
      "md"   [:div {:class (c [:px 0] [:py 4] [:bg :white])}]
      [:div "Unknown format " (:format v)
       [:pre (:content v)]])

    (and (vector? v) (= :div (first v)))
    [:div.markdown-body (eval v)]

    (keyword? v)
    [:span {:class (c [:text :green-600])} v]

    (symbol? v)
    [:a {:href (str v) :class (cond ;;(zd.parser/get-doc ztx v) (c [:text :blue-500])
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
   (sort-by first (:zdb @ztx))
   (reduce (fn [acc [nm doc]]
             (let [parts (interpose :items (str/split (name nm) #"\."))]
               (assoc-in acc parts {:title (or (get-in doc [:resource :title]) nm)
                                    :href (str nm)
                                    :errors (when-let [err (:zen/errors doc)]
                                              (count err))})))
           {})))



(defn render-items [item & [k depth]]
  [:div {:id  (str/lower-case k)
         :class ["closable"]}
   [(if (:href item)
      :a :div) {:href (:href item)
                :class (c :inline-block :flex :items-center [:p 1] :rounded [:hover :cursor-pointer [:bg :gray-200]])}
    (when (:items item)
      [:span {:class (c [:hover :rounded  :cursor-pointer [:bg :gray-300]] :text-lg [:mr 0.5])}
       [:i.fas.fa-caret-down.toggler.rotateToggler]])
    (when (and depth (not (seq (:items item))))
      [:span {:class (c [:mr 0.5] {:padding-left "4px"
                                   :padding-right "4px"
                                   :padding-top "2px"
                                   :padding-bottom "2px"})}])
    (if (:href item)
      [:span {:class (when (:href item) (c [:text :blue-500]))}
       (:title item) (when-let [e (:errors item)]
                       [:span {:class [(c [:text :red-500] :text-xs [:px 1])]}
                        e])]
      [:div {:class (c :inline-block)} k])]
   (into [:div {:class ["closed" "closableContent" "pl-4"]}
          (let [node-content
                (for [[k it] (->> (:items item)
                                  (sort-by :title))]
                  (render-items it k true))]
            node-content)])])

(defn navigation [ztx doc]
  [:div {:class (c [:px 4] [:w 80] [:text :gray-600]  :text-sm)}
   (into [:div]
         (for [[k it] (->> (build-tree ztx doc)
                           (sort-by :title))]
           (render-items it k)))])


;; (defmethod render-key [:zen/errors]
;;   [ztx k v]
;;   [:div {:class (c [:bg :red-100]  [:p 2])}
;;    [:h3 {:class (c [:text :red-700] [:my 1] :text-xl)} "zen/errors"]
;;    [:div.markdown-body
;;     [:ul
;;      (for [e v]
;;        [:li {:class (c [:text :red-500] :text-xs)}
;;         (:message e)
;;         "  @ "
;;         (pr-str (:path e))
;;         (when (:schema e)
;;           (str  " by "        (pr-str (:schema e))))])]]])

;; (defmethod render-key :default
;;   [ztx k v]
;;   (if (:format v)
;;     [:div  {:class (c {:border-bottom "1px solid #eaecef"})}
;;      [:div {:class (c [:text :gray-600] :text-sm {:font-weight "400"})}
;;       (str/join "~" k)]
;;      (render-value ztx k v)]
;;     [:div {:class (c :flex [:space-x 2] [:pt 1] [:pb 0.5]
;;                      :items-center
;;                      {:border-bottom "1px solid #eaecef"})}
;;      [:div {:class (c [:text :gray-600] :text-sm [:w 40] {:font-weight "400"})}
;;       (str/join "~" k)]
;;      (render-value ztx k v)]))

(def key-class (c [:text :orange-600] {:font-weight "400"}))

(defmulti do-format (fn [ztx fmt block] fmt))


(defmethod do-format
  :md
  [ztx fmt {data :data ann :annotations}]
  [:div {:class (c [:px 0] [:py 4] [:bg :white])}
   (zd.zentext/parse-block ztx data)])

(defmethod do-format
  :h
  [ztx fmt {data :data ann :annotations pth :path}]
  [:div {:class (c [:px 0] [:py 4] [:bg :white])}
   [:h2 (pr-str pth)]
   (zd.zentext/parse-block ztx data)])

(defmethod do-format
  :edn
  [ztx fmt {data :data ann :annotations}]
  [:div {:class (c [:px 0] [:py 4] [:bg :white])}
   [:pre
    (clj-yaml.core/generate-string data)]])

(defmulti do-block (fn [ztx k block] k))

(defmethod do-block :default [& _] nil)



(defmethod do-format
  :default
  [ztx fmt {data :data ann :annotations}]
  [:div (pr-str data)])


(defmulti render-key     (fn [ztx {pth :path}] pth))
(defmulti render-block   (fn [ztx {{blk :block} :annotations}] (keyword blk)))
(defmulti render-content (fn [ztx {{cnt :content} :annotations}] (keyword cnt)))

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
    (string? data) data
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


(defn page [ztx {doc :doc}]
  [:div {:class (c [:w 260] [:bg :white] [:py 4] [:px 8] :shadow-md)}
   (->>
    (for [block doc]
      (or (render-key ztx block)
          (render-block ztx block)))
    (into [:div {:class (c )}]))])



(defn links [ztx doc]
  (let [grouped-refs (zd.db/group-refs-by-attr ztx (:zd/name doc))]
    (when (seq grouped-refs)
      [:div {:class (c [:text :gray-600])}
       (->>
        (for [[attr links] grouped-refs]
          [:div {:class (c [:py 4] :text-sm)}
           [:span (->> attr (map (comp str/capitalize name #({:# "["} % %))) (str/join " "))]
           (for [l links]
             [:div {:class (c :flex :flex-col)}
              [:a {:href l
                   :class (c [:text :blue-500])}
               l]])])
        (into [:div {:class (c [:bg :white] [:py 2] [:px 4] :shadow-md)}
               [:span {:class (c [:text :black] :font-bold)} "Referenced By"]]))])))


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

(defn render-not-found [ztx sym]
  (->> [:div {:class (c [:p 4] :flex [:space-x 4])}
        (navigation ztx nil)
        [:div {:class (c [:w 260]  [:py 4] [:px 8] [:text :gray-600])}
          (str "No page for '" sym)]]
       (layout ztx)
       to-html))

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
