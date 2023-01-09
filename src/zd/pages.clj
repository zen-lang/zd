(ns zd.pages
  (:require
   [zd.zentext]
   [zd.db]
   [zd.methods]
   [zd.parse]
   [clj-http.client :as client]
   [zd.impl]
   [hiccup.core]
   [hiccup.page]
   [hiccup.util]
   [markdown.core]
   [clojure.string :as str]
   [stylo.core :refer [c]]
   [stylo.rule :refer [rule join-rules]]
   [garden.core]
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [zd.icons]
   [zd.external-auth]))

(defmethod rule :grid-template-areas
  [_ & xs]
  [[:& {:grid-template-areas (apply pr-str xs)}]])


(defn to-html [x] (hiccup.core/html x))

(def closed-node-style (c [:bg :red-500]))

(defn c* [& args]
  (join-rules args))

(def common-style
  [:body {:font-family "sohne, \"Helvetica Neue\", Helvetica, Arial, sans-serif;" :padding "0" :margin "0"}
   [:h1 (c* :border-b {:font-size "32px" :margin-top "10px" :margin-bottom "16px" :font-weight "600"})]
   [:h2 (c* :border-b {:font-size "24px" :margin-top "20px" :margin-bottom "14px" :font-weight "600" :line-height "30px"})]
   [:h3 (c* :border-b {:font-size "20px" :margin-top "20px" :margin-bottom "14px" :font-weight "600" :line-height "25px"})]
   [:h4 (c* {:font-size "16px" :margin-top "20px" :margin-bottom "14px" :font-weight "600" :line-height "20px"})]
   [:h5 (c* {:font-size "14px" :margin-top "20px" :margin-bottom "14px" :font-weight "600" :line-height "16px"})]
   [:.menu-item (c* :cursor-pointer [:hover [:bg :blue-700]])]
   [:ul (c* [:ml 4] [:mb 4])
    {:list-style "inside"
     :line-height "24px"}
    [:li {:display "list-item"}]
    [:ul (c* [:mb 0])]]
   [:ol (c* [:ml 4]
            {:list-style "disk inside"
             :line-height "24px"})
    [:li (c* [:my 1]
             {:display "list-item"
              :list-style "decimal"})]
    [:ol (c* [:ml 4])]]

   [:p (c* [:my 2] {:line-height "1.5rem"})]

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
   [:.badge
    [:p {:margin 0}]]

   [:.visible {:visibility "visible"}]
   [:.pl-4  {:padding-left "1rem"}]
   [:.toggler-arrow {:padding-left "4px"
               :padding-right "4px"
               :padding-top "2px"
               :padding-bottom "2px"}]
   [:.rotateToggler {:transform "rotate(-90deg)"}]
   [:.searchContainer {:position "fixed"
                       :width "90%"
                       :height "100%"
                       :top 0
                       :transition "transform 0.3s 0.3s"}]


   [:.mindmap
    [:.node
     [:circle {:fill "#aaa"}]
     [:text {}]]
    [:.node--internal [:circle {:fill "#999"}]]
    [:.link {:fill "none"
             :stroke "#aaa"
             :stroke-opacity "0.4"
             :stroke-width "1.5px"}]]

   [:.zd-toggle [:.zd-content {:height "0"
                               :transform "scaleY(0) "
                               :transform-origin "top"
                               :transition "all 0.26s ease"}]]
   [:.zd-toggle.zd-open
    [:.zd-content {:transform "scaleY(1)"
                   :height "auto"}]
    [:.zd-block-title [:.fas {:transform "rotate(90deg)"
                              :transition "all 0.26s"}]]]])


(defn styled-div [cnt]
  [:div
   [:style (stylo.core/compile-styles @stylo.core/styles)]
   [:style (garden.core/css common-style)]
   cnt])

(defn layout [ztx content & [doc]]
  (zd.db/index-refs ztx)
  [:html
   [:head
    [:style (stylo.core/compile-styles @stylo.core/styles)]
    [:style (garden.core/css common-style)]
    [:style "*, ::before, ::after {overflow-x: auto;}
@media (max-width: 700px) {
 #aside {display: none;}
 #topbar {display: none;}
}

"]
    [:meta {:charset "UTF-8"}]
    (when-let [title (->> (:doc doc)
                          (filter #(= [:title] (:path %)))
                          (first)
                          (:data))]
      [:title title])
    [:link {:href "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/styles/default.min.css", :rel "stylesheet"}]
    [:link {:href "/js/fa/css/all.min.css", :rel "stylesheet"}]
    ;; [:link  {:href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"  :rel "stylesheet"}]
    [:link  {:href "/js/spinner.css"  :rel "stylesheet"}]
    [:script {:src "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/highlight.min.js"}]
    [:script {:src "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/languages/clojure.min.js"}]
    [:script {:src "//cdn.jsdelivr.net/npm/lodash@4.17.21/lodash.min.js"}]
    [:script {:src "/js/d3.js"}]
    [:script {:src "/js/mindmap.js"}]
    [:script {:src "/js/zendoc.js"}]
    ;; [:script {:src "/js/zeneditor.js"}]
    [:script {:src "/js/quick-score.min.js"}]
    [:script {:src "/js/editor.js"}]
    [:script "hljs.highlightAll()"]]
   [:body {:class (c {})}
    [:div#overlay
     {:class (c :fixed [:top 0] [:left 0] :h-min-full :w-min-full :overflow-y-hidden
                {:z-index 1} {:background-color "rgba(0, 0, 0, 0.4)"} {:visibility "hidden"})}]
    content
    [:script (format "\nconst searchData = %s;\n%s"
                     (json/encode (zd.db/index-refs ztx))
                     (slurp (io/resource "js/tree.js")))]

    [:script {:src "https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"}]
    [:script "mermaid.initialize({startOnLoad:true});"]]])

(defn build-tree [ztx doc]
  (->>
   (sort-by first (:zdb @ztx))
   (filter #(not (contains? #{'logo} (get % 0))))
   (reduce (fn [acc [nm doc]]
             (let [parts (interpose :items (str/split (name nm) #"\."))]
               (assoc-in acc parts {:title (or (get-in doc [:resource :title])
                                               (last parts))
                                    :zd/name nm
                                    :avatar    (or (get-in doc [:resource :avatar]) (get-in doc [:resource :logo]))
                                    :icon      (get-in doc [:resource :icon])
                                    :menu-order (get-in doc [:resource :menu-order] 10)
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


(defn menu-item-sort [[_ x]] (format "%07d %s" (get x :menu-order 10) (:title x)))

(defn item-avatar [item]
  (if-let [ava (:avatar item)]
      [:img {:class (c [:w 4] [:h 4] [:mr 1] {:border-radius "100%"}) :src ava}]
      (let [ico (or (:icon item) [:fa-regular :fa-file])]
        [:span {:class (c [:w 4] [:h 4] :flex :items-center :justify-center :text-xs [:mr 1])}
         [:i {:class (str/join " " (map name ico))}]])))

(defn item-title [item k]
  [:span {:class (c [:ml 0.5] :flex-1)} (or (:title item) (:href item) k)
     (when-let [e (:errors item)] [:span {:class (->> [(c [:text :red-500] :text-xs [:px 1])]
                                                      (str/join " "))} e])])
(defn render-items [doc item & [k]]
  [:div {:id  (str/lower-case k) :class "closable"}
   [:a.toggler {:href (when-not (:broken item) (:href item))
        :class (->> [(c :inline-block :flex :items-center 
                        [:pl 2]
                        [:py 2]
                        [:mr 2]
                        :rounded
                        [:hover :cursor-pointer [:text :blue-600]])
                     (when (:broken item) (c [:text :red-500]))
                     (when (= (:zd/name doc) (:zd/name item))
                       (c [:bg :white] [:text :gray-700] {:border-right "none"}))]
                    (filterv identity)
                    (mapv name)
                    (str/join " "))}
    (item-avatar item)
    (item-title item k)

    (when (:items item)
      [:span {:class (c [:w 10] [:text :gray-500]
                        :cursor-pointer
                        :flex
                        :justify-center
                        {:font-size "12px"}
                        :overflow-y-hidden
                        [:hover :rounded
                         [:text :red-600]])}
       [:i.fas.toggler-arrow.fa-chevron-down.toggler.rotateToggler]])]
   (into [:div {:class (->> ["closed" "closableContent" (name (c :border-l [:ml 4]))]
                            (str/join " "))}
          (let [node-content
                (for [[k it] (->> (:items item)
                                  (sort-by menu-item-sort))]
                  (render-items doc it k true))]
            node-content)])])

(defn navigation [ztx doc]
  [:aside#aside {:class (c [:text :gray-600] [:px 6] [:py 4]  :text-sm
                           {:max-width "300px"})}
   [:div {:id "nav-files"}
    (for [[k it] (->> (build-tree ztx doc)
                      (sort-by menu-item-sort))]
      (render-items doc it k))]])


(defn breadcrumb [_ztx name & [page]]
  (let [parts (str/split (str name) #"\.")]
    (-> 
     (->> (range (count parts))
          (mapv (fn [x]
                  (let [pth (into [] (take (inc x) parts))
                        nm  (str/join "." pth)]
                    [:a {:href (str "/" nm)
                         :class (c [:text :blue-500] [:px 2] {:border-right "1px solid #ddd"})}
                     (last pth)])))
          (into [:div {:class (c :flex :flex-1 :items-center)}]))
     (conj
      [:a {:class (c [:mx 4] [:text :green-600] [:hover [:text :green-700]])
           :href (str name "/" "edit" "?" (get-in page [:request :query-string]))}
       [:i.fas.fa-edit]]))))


(defn links [ztx link-groups]
  [:div {:class (c [:text :gray-600])}
   (when (seq link-groups)
     (->> link-groups
          (reduce-kv (fn [acc k v]
                       (update acc (first k) (fnil into []) v))
                     {})
          (map (fn [[attr links]]
                 [:div {:class (c [:py 2] :text-sm)}
                  [:div {:class (c [:text :gray-600] :border-b [:mb 2] {:font-weight "600"})}
                   attr]
                  [:div
                   (for [l (sort links)]
                     [:div {:class (c [:py 0.5])} (zd.impl/symbol-link ztx l)]
                     #_[:a {:href l :class (c :block [:py 0.5] [:text :gray-700] [:hover [:text :gray-800]])} l])]]))
          (into [:div {:class (c  [:py 2] [:px 0])}
                 [:span {:class (c [:text :black] :font-bold)} "Referenced By"]])))])

(def page-cls (c [:mr 12] {:min-width "30em" :max-width "50em"}))
(def full-page-cls (c [:mr 12] ))

(defn page-content [ztx {doc :doc req :request res :resource :as page}]
  [:div {:class (if (= :full (:page/width res)) full-page-cls page-cls)}
   (->>
    (for [block doc]
      (let [block (assoc block :page page)]
        (or (zd.methods/render-key ztx block)
            (zd.methods/render-block ztx block)))))])


(defn page [ztx {doc :doc req :request res :resource :as page}]
  [:div {:class (c :inline-flex [:py 4] [:px 8] :flex-1 )}
   [:div {:class (if (= :full (:page/width res)) full-page-cls page-cls)}
    (breadcrumb ztx (:zd/name page) page)
    [:div {:class (c [:bg :white] {:color "#3b454e"})}
     (page-content ztx page)]]
   [:div {:class (c {:min-width "15em"})}
    (links ztx (:backrefs page))]])

(defn edit-page [ztx {doc :doc _res :resource :as page}]
  [:div {:class (c [:mb 4])}
   (->>
    (for [block doc]
      (let [block (assoc block :page page)]
        (or (zd.methods/render-key ztx block)
            (zd.methods/render-block ztx block)))))])


(defn search []
  [:div {:class (c :text-sm
                   [:text :gray-600]  [:hover [:bg :white]])}
   [:div#searchButton {:class (c :flex [:space-x 2] :items-baseline
                                 :text-l
                                 {:transition "color 0.2s ease"}
                                 [:hover :cursor-pointer [:text :black]])}
    [:span [:i.fas.fa-search]]
    [:span "[alt+k]"]]])


(defn search-container [_ztx _doc]
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


(defn topbar
  [ztx doc]
  (let [src (get (:zdb @ztx) 'logo)
        logo (get-in src [:resource :logo])
        title (get-in src [:resource :title])]
    [:div#topbar {:class (c :flex :items-center :border-b)}
     [:div {:class (c  [:py 4])}
      [:a {:href "/readme"}
       [:div {:class (c [:px 6] :flex :items-center [:border-r 2])}
        [:div {:class (c [:w 6] [:h 6] [:mr 4])}
         (when logo
           [:image {:src logo}])]
        [:div {:class (c :font-bold)}
         (when title
           title)]]]]
     [:div {:class (c  [:py 4])}
      [:div {:class (c :h-full [:px 6] [:py 1] [:border-l 2] :items-center :justify-start :flex)}
       (search)]]]))

;; :grid [:grid-template-areas "navigation content"]
;; {:grid-template-columns  "300px 1fr"
;;  :margin-top "80px"}

(defn generate-page [ztx doc]
  (let [link-groups (zd.db/group-refs-by-attr ztx (:zd/name doc))]
    [:div {:class (c )}
     ;; (topbar ztx doc)
     [:div {:class (c :flex :items-top)}
      (navigation ztx doc)
      (page ztx (assoc doc :backrefs link-groups))]
     [:div {:class (c {:position "absolute" :top "1rem" :right "2rem"})}
      (search)]
     (search-container ztx doc)]))

(defn generate-edit-page [ztx doc]
  (page ztx doc))

(defn zen-page [_ztx doc]
  [:div {:class (c [:w 260] [:bg :white] [:py 4] [:px 8] :shadow-md)}
   (when-let [t (:zen/name doc)]
     [:div {:class (c :flex :border-b [:pt 2] [:pb 1])}
      [:div {:class (c :flex-1 [:text :gray-800] :font-bold  {:font-size "1.5rem"})} t]])
   [:pre (pr-str doc)]])


(defn generate-zen-page [ztx doc]
  [:div {:class (c [:p 4] :flex [:space-x 4])}
   (navigation ztx doc)
   (zen-page ztx doc)])

(def base-class
  (c [:px 3] [:py 2]
     :inline-flex
     :items-center
     :cursor-pointer
     [:leading-relaxed]
     :border
     :rounded
     :whitespace-no-wrap
     [:bg :white]
     [:space-x 1]
     [:text :gray-700]
     :transition-all [:duration 200] :ease-in-out
     [:focus :outline-none :shadow-outline]
     [:pseudo ":not(:disabled)"
      [:hover [:text :blue-500] [:border :blue-500]]
      [:active [:text :blue-800] [:border :blue-800]]]
     [:disabled [:text :gray-500] [:bg :gray-200] [:border :gray-400] :cursor-not-allowed]))


(defn find-template [ztx nm]
  (when nm
    (let [parts (str/split (str nm) #"\.")]
      (loop [parts parts]
        (when (not (empty? parts))
          (let [f (str "docs/" (str/join "/" (butlast parts)) "/_template.zd")]
            (if (.exists (io/file f))
              (slurp f)
              (recur (butlast parts)))))))))

(def default-tpl ":title \"\"\n:tags #{}")
(defn generate-editor [ztx doc]
  (let [raw (if-let [pth (:zd/path doc)]
              (slurp (:zd/path doc))
              (or (find-template ztx (:zd/name doc))
                  default-tpl))]
    [:div
     [:div {:class (c :flex [:h "100%"])}
      [:div {:class (c [:p 0] [:w-min 150] :border {:position "relative"})}
       [:textarea {:class (c [:w "100%"] [:h "100%"] [:p 4] :rounded {:resize "none"})
                   :id "edit-page"} raw]
       [:div#spinner {:class (c  {:position "absolute" :bottom "10px" :left "10px"})}"..."]
       [:div {:class (c {:font-size "10px" :opacity 0.5 :position "absolute" :top "2px" :left "2px"})} (str (:zd/name doc))]
       [:div {:class (c :ml-auto
                        [:px 4] [:py 2] :cursor-pointer
                        [:bg :blue-500] [:text :white]
                        [:hover [:bg :blue-600] [:text :white]]
                        {:position "absolute" :bottom "10px" :right "10px"})
              :onclick "savePreview()"} "Save"]]
      [:div {:class (c :border [:p 4] :flex-1) :id "edit-preview"}]]]))



(defn render-page [ztx doc]
  (->> (layout ztx (generate-page ztx doc) doc)
       (to-html)))

(defn preview [ztx text]
  (hiccup.core/html (page-content ztx (zd.parse/parse ztx text))))

(defn editor [ztx doc]
  (let [text (if (:zd/path doc) (slurp (:zd/path doc))
              (or (find-template ztx (:zd/name doc))
                  default-tpl))
        symbols (->> (:zdb @ztx)
                     (mapv (fn [[k {{ico :icon logo :logo tit :title} :resource}]]
                             {:title tit
                              :name k
                              :logo logo
                              :icon ico})))
        keypaths (->> (:zd/keys @ztx)
                      (mapv (fn [x] {:name x})))
        zendoc {:text text
                :symbols symbols
                :keys keypaths
                :icons (->> zd.icons/icons
                            (mapv (fn [x] {:name x})))
                :preview (preview ztx text)
                :doc (:zd/name doc)}]
    [:script "var zendoc="(cheshire.core/generate-string zendoc)]))

(defn render-edit-page [ztx doc]
  (->> (editor ztx doc)
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
