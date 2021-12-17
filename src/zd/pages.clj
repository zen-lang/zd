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
   [stylo.rule  :refer [join-rules]]
   [cheshire.core :as json]))

(defn to-html [x] (hiccup/html x))

(def closed-node-style (c [:bg :red-500]))

(defn c* [& args]
  (join-rules args))

(def common-style
  [:body {:font-family "sohne, \"Helvetica Neue\", Helvetica, Arial, sans-serif;"}
   [:h1 (c* [:mb 2] {:font-size "46px" :font-weight "700" :border-bottom "1px solid #f1f1f1"})]
   [:h2 (c* [:mb 2] {:font-size "32px" :font-weight "700" :line-height "40px" :border-bottom "1px solid #f1f1f1"})]
   [:h3 (c* [:mb 2] {:font-size "24px" :font-weight "700" :line-height "36px" :border-bottom "1px solid #f1f1f1"})]
   [:ul {:list-style "inside"
         :line-height "24px"}
    [:li {:display "list-item"}]
    [:ul {:margin-left "2rem"}]]
   [:ol {:list-style "disk inside"
         :line-height "24px"}
    [:li {:display "list-item"}]
    [:ol (c* [:ml 4])]]
   [:p (c* [:my 1])]
   [:.hljs (c* [:bg :gray-100] :shadow-sm
               :border)]
   [:pre {:margin-top "1rem" :margin-bottom "1rem"}]
   [:.closed {:display "none"}]
   [:.bolder (c* :font-bold)]
   [:.nlSaver {:white-space "pre-wrap"}]
   [:.searchResultContainer (c* [:px 6] [:py 3] :flex :flex-col
                                [:hover :cursor-pointer [:bg :gray-200]])]
   [:.searchResultContainerRow (c* :flex)]
   [:.searchResultContainerSummaryRow (c* :border-t)]
   [:.searchResultContainerVBar (c* [:h "30px"] [:w "2px"]
                                    :rounded [:mr 2] [:bg :blue-500])]
   [:.visible {:visibility "visible"}]
   [:.pl-4  {:padding-left "1rem"}]
   [:.toggler {:padding-left "4px"
               :padding-right "4px"
               :padding-top "2px"
               :padding-bottom "2px"}]
   [:.rotateToggler {:transform "rotate(-90deg)"}]
   [:.searchContainer {:position "fixed"
                       :width "90%"
                       :height "100%"
                       :top 0
                       :transition "transform 0.3s 0.3s"}]
   ])


(defn layout [ztx content]
  (zd.db/index-refs ztx)
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
   [:body {:class (c [:bg :gray-100] :w-max-full)}
    [:div#overlay
     {:class (c :fixed [:top 0] [:left 0] :h-min-full :w-min-full :overflow-y-hidden
                {:z-index 1} {:background-color "rgba(0, 0, 0, 0.4)"} {:visibility "hidden"})}]
    content
    [:script (format "\nconst searchData = %s;\n%s"
                     (json/encode (zd.db/index-refs ztx))
                     (slurp "./src/js/tree.js"))]]])




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
           (zd.methods/render-block ztx block))))]])

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


(defn search [ztx doc]
  [:div {:class (c :text-sm [:bg :white] [:py 2] [:px 4] :shadow-md
                   [:text :gray-500])}
   [:div#searchButton {:class (c :flex [:space-x 2] :items-baseline
                                 {:transition "color 0.2s ease"}
                                 [:hover :cursor-pointer [:text :black]])}
    [:span [:i.fas.fa-search]]
    [:span "Search... (alt + k)"]]])


(defn search-container [ztx doc]
  [:div#searchContainer
   {:class (c :fixed {:z-index 2} [:w "30%"] :h-min-screen [:bg :gray-100] [:top 0] [:right 0] {:transition "transform 0.3s 0.3s" :visibility "hidden"} [:my 0])}
   [:div {:class (c :flex :flex-col)}
    [:div {:class (c :flex :items-center [:bg :white] [:p 1.5])}
     [:span {:class (c [:mr 2] [:text :gray-500])} [:i.fas.fa-search]]
     [:input#searchInput
      {:placeholder "Search..."
       :class (c [:h "30px"] :flex-1 :text-xl
                 [:focus {:outline "none"}]
                 [:placeholder :text-lg :gray-500 :text-xl])}]
     [:span#searchContainerClose
      {:class (c [:text :gray-500] [:mr 2] {:transition "color 0.2s ease"}
                 [:hover :cursor-pointer [:text :black]])} "✕"]]
    [:div#searchResults
     {:class (c :flex :flex-col :h-max-screen :overflow-y-scroll)}]]])


(defn generate-page [ztx doc]
  [:div {:class (c [:p 4] :flex [:space-x 4])}
   (navigation ztx doc)
   (page ztx doc)
   (search-container ztx doc)
   [:div {:class (c [:w "15%"] :flex :flex-col [:space-y 4])}
    (search ztx doc)
    (links ztx doc)]])

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


