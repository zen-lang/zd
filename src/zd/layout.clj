(ns zd.layout
  (:require
   [zd.methods :as methods]
   [cheshire.core :as json]
   [garden.core]
   [zen-web.core :as web]
   [stylo.core :refer [c]]
   [stylo.rule :refer [rule join-rules]]
   [clojure.string :as str]))

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

(defn build-navigation [ztx]
  ;; TODO think about caching navigation tree
  (let [resources
        (reduce (fn [acc [k v]]
                  (assoc acc k (select-keys v [:title :tags :icon :logo :desc])))
                {}
                (:zdb @ztx))

        tree
        (->> resources
             (sort-by (fn [[k _]] (name k)))
             (reduce (fn [acc [k _]]
                       (let [path (str/split (name k) #"\.")]
                         ;; TODO add menu order from metadata to tree
                         (assoc-in acc path {:name k})))
                     {}))

        resources
        (->> resources
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
                 (mapv (fn [[k v]] (:name v)))
                 (into ['index]))}))

(defn sidebar-layout [ztx content]
  [:html
   [:head
    [:style (stylo.core/compile-styles @stylo.core/styles)]
    [:style (garden.core/css common-style)]
    [:style "*, ::before, ::after {overflow-x: auto;}
@media (max-width: 700px) {
 #aside {display: none;}
 #topbar {display: none;}} "]
    [:meta {:charset "UTF-8"}]
    ;; TODO fix title
    #_(when-let [title (->> (:doc page)
                            (filter #(= [:title] (:path %)))
                            (first)
                            (:data))]
        [:title title])
    [:link {:href "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/styles/default.min.css", :rel "stylesheet"}]
    [:link {:href "/static/js/fa/css/all.min.css", :rel "stylesheet"}]
    [:link  {:href "/static/js/spinner.css"  :rel "stylesheet"}]
    [:script {:src "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/highlight.min.js"}]
    [:script {:src "//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/languages/clojure.min.js"}]
    [:script {:src "//cdn.jsdelivr.net/npm/lodash@4.17.21/lodash.min.js"}]
    [:script {:src "/static/js/d3.js"}]
    [:script {:src "/static/js/mindmap.js"}]
    [:script {:src "/static/js/zendoc.js"}]
    [:script {:src "/static/js/quick-score.min.js"}]
    [:script {:src "/static/js/core.js"}]
    [:script {:src "/static/js/editor.js"}]]
   [:body {:class (c {})}
    [:div#overlay
     {:class (c :fixed [:top 0] [:left 0] :h-min-full :w-min-full :overflow-y-hidden
                {:z-index 1} {:background-color "rgba(0, 0, 0, 0.4)"} {:visibility "hidden"})}]
    content
    [:script "var zd={};"]
    [:script "zd.nav=" (json/generate-string (build-navigation ztx))]
    [:script "hljs.highlightAll()"]]])

(defmethod methods/layout 'zd/sidebar
  [ztx config content page]
  ;; TODO emit zen event
  (sidebar-layout ztx content))
