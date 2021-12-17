(ns zd.pages
  (:require
   [zen.core :as zen]
   [zd.zentext]
   [zd.db]
   [zd.methods]
   [zd.impl]
   [hiccup.core :as hiccup]
   [hiccup.page]
   [hiccup.util]
   [markdown.core]
   [clojure.string :as str]
   [stylo.core :refer [c c?]]
   [garden.core]
   [stylo.rule  :refer [join-rules]]))

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
   [:p (c* [:my 2])]
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


(defn build-tree [ztx doc]
  (->>
   (sort-by first (:zdb @ztx))
   (reduce (fn [acc [nm doc]]
             (let [parts (interpose :items (str/split (name nm) #"\."))]
               (assoc-in acc parts {:title (or (get-in doc [:resource :title]) nm)
                                    :href (str nm)
                                    :errors (when-let [err (:zen/errors doc)]
                                              (count err))}))) {})))



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

(def key-class (c [:text :orange-600] {:font-weight "400"}))


(defn page [ztx {doc :doc :as page}]
  [:div {:class (c [:w 260] [:bg :white] [:py 4] [:px 8] :shadow-md)}
   [:div {:class (c [:mb 4])}
    (->>
     (for [block doc]
       (or (zd.methods/render-key ztx block)
           (zd.methods/render-block ztx block)))
     (into [:div {:class (c )}]))]
   (when (seq (:errors page))
     [:div {:class (c [:bg :red-200] [:border :red-300] [:py 2] [:px 4])}
      [:div {:class (c :font-bold :text-lg [:mb 2])} "Errors"]
      (for [err (sort-by :type (:errors page))]
        [:div {:class (c [:mb 1])}
         [:span {:class (c)} (:message err) " "]
         [:span {:class (c [:text :gray-600])} (str (:path err))]])])])


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


