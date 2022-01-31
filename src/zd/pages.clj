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
   [cheshire.core :as json]
   [clojure.java.io :as io]))

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
   [:.active-nav {:border-bottom "2px solid #666" :color "black"}]
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
                     (slurp (io/resource "js/tree.js")))]]])

(defn build-tree [ztx doc]
  (->>
   (sort-by first (:zdb @ztx))
   (reduce (fn [acc [nm doc]]
             (let [parts (interpose :items (str/split (name nm) #"\."))]
               (assoc-in acc parts {:title (or (get-in doc [:resource :title])
                                               (last parts))
                                    :name      (last parts)
                                    :href (str nm)}))) {})))

(defn build-menu* [ztx {ref :ref :as item} doc]
  (let [res  (zd.db/get-resource ztx ref)
        title (or (:title item) (:title res)  (last (str/split (str ref) #"\.")))]
    (merge item
           {:title title
            :broken (nil? res)
            :href (str ref)
            :items (when (and (:menu res) (sequential? (:menu res)))
                     (->> (:menu res)
                          (map-indexed (fn [i x] [i x]))
                          (reduce (fn [acc [i x]]
                                    (assoc acc i (build-menu* ztx x doc)))
                                  {})))})))

(defn build-menu [ztx doc]
  (:items (build-menu* ztx {:ref 'readme} doc)))

(defn render-items [item & [k depth]]
  [:div {:id  (str/lower-case k) :class "closable"}
   [:a {:href (when-not (:broken item) (:href item))
        :class (->> [(c :inline-block :flex :items-center [:py 1] :rounded [:hover :cursor-pointer [:bg :white] [:text :gray-700]])
                     (when (:broken item) (c [:text :red-500]))]
                    (filterv identity)
                    (mapv name)
                    (str/join " "))}
    (if (:items item)
      [:span {:class (c [:w 6] [:hover :rounded  :cursor-pointer [:bg :gray-300]] :text-lg :flex :justify-center)}
       [:i.fas.fa-caret-down.toggler.rotateToggler]]
      [:span {:class (c [:w 6] [:h 6] :flex :items-center :justify-center [:text :gray-300])}
       [:i.fa.fa-file]])

    [:span (or (:title item) (:href item) k)
     (when-let [e (:errors item)] [:span {:class (->> [(c [:text :red-500] :text-xs [:px 1])]
                                                      (str/join " "))} e])]]
   (into [:div {:class (->> ["closed" "closableContent" (name (c :border-l [:ml 3]))]
                            (str/join " "))}
          (let [node-content
                (for [[k it] (->> (:items item)
                                  (sort-by :title))]
                  (render-items it k true))]
            node-content)])])

(def tab-class (c [:p 1] [:text :gray-600] :cursor-pointer
                  {:margin-bottom "-2px"}
                  [:hover [:text :gray-800] {:border-bottom "2px solid #888"}]))

(defn navigation [ztx doc]
  (let [root (zd.db/get-resource ztx 'readme)]
    [:div {:class (c [:pr 4] [:w 80] [:text :gray-600]  :text-sm)}
     [:div  {:class (c [:py 2]  [:mb 2] :flex [:space-x 4] :items-center)}
      [:img {:src "/logo.png" :class (c [:h 8])}]
      [:div {:class (c :text-xl :font-bold)}(:title root)]]
     #_[:div {:class (c :flex [:space-x 2] :border-b :items-baseline [:mb 4])}
      [:div#menuTab {:class ["tab" tab-class "active-nav"] :for "nav-menu"} "Menu"]
      [:div#fileTab {:class ["tab" tab-class] :for "nav-files"} "Files"]]
     #_[:div {:id "nav-menu"  :style "display: none;"}
      (for [[k it] (build-menu ztx doc)]
        (render-items it k))]
     [:div {:id "nav-files"}
      (for [[k it] (->> (build-tree ztx doc)
                        (sort-by :title))]
        (render-items it k))]]))

(def key-class (c [:text :orange-600] {:font-weight "400"}))


(defn breadcrump [ztx name]
  (let [parts (str/split (str name) #"\.")]
    (->> (range (count parts))
         (mapv (fn [x]
                 (let [pth (into [] (take (inc x) parts))
                       nm  (str/join "." pth)]
                   [:a {:href (str "/" nm)
                        :class (c [:text :blue-500] [:px 2] {:border-right "1px solid #ddd"})}
                    (last pth)])))
         (into [:div {:class (c :flex :flex-1)}]))))

(defn page [ztx {doc :doc res :resource :as page}]
  [:div 
   [:div {:class (c :flex [:py 1])}
    (breadcrump ztx (:zd/name page))
    [:div {:class (c :text-sm [:text :gray-600])}
     (:zd/name page)]]
   [:div {:class (c [:w 230]
                    [:w-max 230]
                    [:bg :white] [:py 4] [:px 16] :shadow-md
                    {:color "#3b454e"})}
    
    [:div {:class (c [:mb 4])}
     (->>
      (for [block doc]
        (let [block (assoc block :page page)]
          (or (zd.methods/render-key ztx block)
              (zd.methods/render-block ztx block)))))]]])

(defn links [ztx doc]
  (let [grouped-refs (zd.db/group-refs-by-attr ztx (:zd/name doc))]
    (when (seq grouped-refs)
      [:div {:class (c [:text :gray-600])}
       (->>
        (for [[attr links] grouped-refs]
          [:div {:class (c [:py 2] :text-sm)}
           [:div {:class (c [:text :gray-600] :border-b [:mb 2] {:font-weight "600"})}
            (str/join "" (mapv str attr))]
           (for [l links]
             [:a {:href l :class (c :block [:py 0.5] [:text :gray-700] [:hover [:text :gray-800]])} l])])
        (into [:div {:class (c  [:py 2] [:px 4] )}
               [:span {:class (c [:text :black] :font-bold)} "Referenced By"]]))])))


(defn search [ztx doc]
  [:div {:class (c :text-sm [:py 2] [:px 4] [:text :gray-600] [:border :gray-400] :rounded [:hover [:bg :white]])}
   [:div#searchButton {:class (c :flex [:space-x 2] :items-baseline
                                 {:transition "color 0.2s ease"}
                                 [:hover :cursor-pointer [:text :black]])}
    [:span [:i.fas.fa-search]]
    [:span "Search... (alt + k)"]]])


(defn search-container [ztx doc]
  [:div#searchContainer
   {:class (c :fixed {:z-index 2} [:w "30%"] :h-min-screen [:bg :gray-100] [:top 0]
              [:text :gray-700]
              [:right 0] {:transition "transform 0.3s 0.3s" :visibility "hidden"} [:my 0])}
   [:div {:class (c :flex :flex-col)}
    [:div {:class (c :flex :items-center [:bg :white] [:pb 2] [:px 8] [:py 4])}
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
  [:div {:class (c [:py 6] [:px 8] :flex [:space-x 4])}
   (navigation ztx doc)
   (page ztx doc)
   (search-container ztx doc)
   [:div {:class (c  :flex :flex-col [:space-y 4] :flex-1)}
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


