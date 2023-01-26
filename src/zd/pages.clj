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
   [:.screenshot (c* :shadow-lg :border :rounded [:m 1] [:p 1])]
   [:.zd-search-popup
    (c* :border :rounded
        [:bg :white]
        [:p 4]
        :box-shadow {:z-index 100 :position "absolute"
                     :top "1rem"
                     :left "1rem"
                     :right "1rem"
                     :bottom "1rem"})
    [:.zd-search
     (c* :border :text-xl
         :rounded
         [:px 4] [:py 1]
         {:width "100%"})]
    [:.zd-comments (c* :text-sm [:text :gray-500] [:py 1])]
    [:.zd-results (c* [:mt 2])
     [:.zd-search-item (c* :cursor-pointer [:py 1] [:px 4] :flex [:space-x 2] :items-center [:hover [:bg :gray-200]] {:border "1px solid transparent"})
      [:&.current (c* [:bg :gray-200] :border)]
      [:i (c* [:text :gray-400])]
      [:.zd-search-desc (c* :text-sm [:text :gray-500])]]]]

   [:.zd-menu-item (c* :flex [:space-x 2] :items-center [:py 0.2]  :cursor-pointer [:px 6]
                       {:white-space "nowrap"})
    [:.zd-folder (c* [:text :transparent])]
    [:&:hover (c* [:bg :gray-200])
     [:.zd-folder.zd-empty (c* [:text :gray-500])]
     [:.zd-folder (c* [:text :orange-400])]]
    [:i (c* [:w 5] {:text-align "center"})]]
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
   [:pre {:margin-top "1rem" :margin-bottom "1rem"}]
   [:.bolder (c* :font-bold)]
   [:.badge
    [:p {:margin 0}]]

   [:.visible {:visibility "visible"}]
   [:.pl-4  {:padding-left "1rem"}]
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

(defn normalize-tree [tree]
  (->> tree
       (mapv (fn [[k v]]
               (if (:items v)
                 (update v :items normalize-tree)
                 v)))
       (sort-by (fn [x] [(get x :menu-order 1000) (:zd/name x)]))))

(defn build-navigation [ztx]
  (let [resources (->> (:zdb @ztx)
                       (sort-by first)
                       (reduce (fn [acc [k v]]
                                 (assoc acc k (select-keys (:resource v) [:title :tags :icon :logo :desc :menu-order]))) {}))
        tree (->> resources
                  (sort-by first)
                  (reduce (fn [acc [k v]]
                            (let [path (str/split (name k) #"\.")]
                              (assoc-in acc path (-> (if-let [mo (:menu-order v)] {:menu-order mo} {})
                                                     (assoc :name k)))))
                          {}))
        resources (->> resources
                       (reduce (fn [acc [k v]]
                                 (let [path (str/split (name k) #"\.")
                                       items (dissoc (get-in tree path) :menu-order :name)]
                                   (assoc acc k
                                          (cond-> (assoc v :name k)
                                            (seq items) (assoc :items
                                                               (->> items
                                                                    (sort-by (fn [[k v]] [(or (:menu-order v) 100000) k]))
                                                                    (mapv (fn [[kk v]] (or (:name v) (str k "." kk))))))))))
                               {}))]
    {:resources resources
     :tree tree
     :items (->> (dissoc tree 'index)
                 (sort-by (fn [[k v]] [(get v :menu-order 1000000) k]))
                 (mapv (fn [[k v]] (:name v)))
                 (into ['index]))}))

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
    [:script {:src "/js/core.js"}]
    [:script {:src "/js/editor.js"}]
    ]
   [:body {:class (c {})}
    [:div#overlay
     {:class (c :fixed [:top 0] [:left 0] :h-min-full :w-min-full :overflow-y-hidden
                {:z-index 1} {:background-color "rgba(0, 0, 0, 0.4)"} {:visibility "hidden"})}]
    content
    [:script "var zd={};"]
    [:script "zd.nav="(cheshire.core/generate-string (build-navigation ztx))]
    [:script "hljs.highlightAll()"]
    [:script {:src "https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"}]
    [:script "mermaid.initialize({startOnLoad:true});"]]])



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
  [:div#left-nav {:class (c [:text :gray-600] [:px 0] [:py 0]  :text-sm
                   :border-r
                   [:bg :gray-100]
                   {:height "100vh"
                    :overflow-y "auto"
                    :min-width "300px"
                    :max-width "400px"})}
   [:div#search {:class (c [:px 5] [:py 2] [:bg :gray-200]
                           :flex :items-center
                           [:space-x 2]
                           [:mb 2]
                           :cursor-pointer
                           :border-b [:hover [:text :blue-500]])}
    [:i.fa-solid.fa-magnifying-glass ]
    [:span "[ctrl-k]"]]
   [:aside#aside
    {:class (c [:text :gray-600] [:px 0] [:py 0]  :text-sm {})}]])

(defn breadcrumb [ztx name page]
  (let [doc-tree (build-tree ztx page)
        parts (str/split (str name) #"\.")
        layout
        (->> (range (count parts))
             (mapcat (fn [x]
                       (let [pth (into [] (take (inc x) parts))
                             nm  (str/join "." pth)]
                         [[:a {:href (str "/" nm)
                               :class (c [:text :blue-500] [:px 2] )}
                           (last pth)]
                          [:a {:data-dir nm
                               :href (str "/" nm)
                               :class (c [:px 2] [:pr 4] :cursor-pointer [:text :orange-500]
                                         {:border-right "1px solid #ddd"}
                                         [:hover [:text :orange-600]])}
                           [:i.fa-solid.fa-folder]]])))
          (into [:div {:class (c :flex :flex-1 :items-center)}]))

        edit-btn
        [:a {:class (c [:mx 4] [:text :green-600] [:hover [:text :green-700]])
             :href (str name "/" "edit" "?" (get-in page [:request :query-string]))}
         [:i.fas.fa-edit]]

        doc-node
        (get-in doc-tree (interpose :items parts))]
    (cond-> (conj layout edit-btn)

(contains? doc-node :items)
(into [[:a {:class (c [:mx 4] [:text :green-600] [:hover [:text :green-700]])
                  :href (str name "." "_draft" "/"  "edit" "?" (get-in page [:request :query-string]))}
              [:i.fas.fa-plus]]
             [:a {:class (c [:mx 4] [:text :green-600] [:hover [:text :green-700]])
                  :href (str name "." "_template" "/" "edit" "?" (get-in page [:request :query-string]))}
              [:span "Template"]]]))))

;; TODO remove me later
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
                     [:div {:class (c [:py 0.5])} (zd.impl/symbol-link ztx l)])]]))
          (into [:div {:class (c  [:py 2] [:px 0])}
                 [:span {:class (c [:text :black] :font-bold)} "Referenced By"]])))])

(def page-cls (c [:mr 12] :flex-1 {:min-width "30em"} [:p 6]))
(def full-page-cls (c :flex-1 [:mr 12] [:p 4]))

(defn page-content [ztx {doc :doc req :request res :resource :as page}]
  [:div {:class (if (= :full (:page/width res)) full-page-cls page-cls)}
   (for [block doc]
     (let [block (assoc block :page page)]
       (or (zd.methods/render-key ztx block)
           (zd.methods/render-block ztx block))))])

(defn page [ztx {doc :doc req :request res :resource :as page}]
  (let [infset #{[:zd/back-links]
                 [:zd/broken-links]}
        [attrs inferred] (->> doc
                              (partition-by #(contains? infset (:path %))))]
    [:div {:class (c :flex :flex-1)}
     [:div {:class (if (= :full (:page/width res)) full-page-cls page-cls)}
      (breadcrumb ztx (:zd/name page) page)
      [:div {:class (c [:bg :white] {:color "#3b454e"})}
       (page-content ztx (assoc page :doc attrs))]]
     (let [[back-links broken-links]
           (->> inferred
                (sort-by #(first (:path %))))]
       (when (or (seq back-links) (seq broken-links)) 
         [:div {:class (c [:bg :gray-100] [:p 6] :border-l {:height "100vh"
                                                            :overflow-y "auto"
                                                            :min-width "15em"
                                                            :max-width "35em"})}
          (zd.methods/render-key ztx back-links)
          (zd.methods/render-key ztx broken-links)]))]))

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
    [:div {:class (c :flex :items-top :w-full)}
     (navigation ztx doc)
     [:div#page {:class (c :flex-1 {:height "100vh" :overflow-y "auto"})}
      (page ztx (assoc doc :backrefs link-groups))]]))

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

(def filename-tpl "^hide\n;; enter file name. required\n:zd/filename \"\"\n\n")

(defn render-page [ztx doc]
  (if (get-in doc [:request :headers "x-body"])
    (->> (page ztx doc)
         (to-html))
    (->> (layout ztx (generate-page ztx doc) doc)
         (to-html))))

(defn preview [ztx text page]
  (hiccup.core/html (page-content ztx (merge page (zd.parse/parse ztx text)))))

(defn editor [ztx doc]
  (let [text (if (:zd/path doc)
               (slurp (:zd/path doc))
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
        docname (-> (str (:zd/name doc))
                    (str/split #"\.")
                    last)
        zendoc {:text (if (= "_draft" docname) (str filename-tpl text) text)
                :symbols symbols
                :keys keypaths
                :icons  zd.icons/icons
                :annotations (->> (keys (methods zd.methods/annotation))
                                  (mapv (fn [x]
                                          {:name (str "^" (name x))
                                           :title (name x)})))
                :preview (preview ztx text doc)
                :doc (:zd/name doc)}]
    [:script "var zendoc=" (cheshire.core/generate-string zendoc)]))

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
