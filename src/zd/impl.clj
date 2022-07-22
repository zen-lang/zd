(ns zd.impl
  (:require
   [clojure.string :as str]
   [stylo.core :refer [c]]
   [zd.db]
   [zd.zentext]
   [sci.core]
   [clj-yaml.core]
   [clojure.pprint]
   [markdown.core]
   [cheshire.core]
   [zd.methods :refer [annotation inline-method inline-function render-block render-content render-key process-block key-data]]))

(defmethod annotation :collapse
  [nm params]
  {:collapse (or params {})})

(defmethod annotation :default
  [nm params]
  (assoc {:content (keyword nm)} (keyword nm) params))

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

(defn get-parent [ztx res]
  (when-let [nm (:zd/name res)]
    (let [pn (->> (str/split (str nm) #"\.")
                  (butlast)
                  (str/join "."))]
      (when-not (str/blank? pn)
        (or (zd.db/get-resource ztx (symbol pn))
            {:zd/name (symbol pn)})))))

(defn resolve-icon [ztx res]
  (if-let [ava (or (get-in res [:avatar]) (get-in res [:logo]))]
    {:type :img :img ava}
    (if-let [icon (get res :icon)]
      {:type :ico :icon icon}
      (when-let [parent (get-parent ztx res)]
        (resolve-icon ztx parent)))))

(defn symbol-link [ztx s]
  (if-let [res (zd.db/get-resource ztx (symbol s))]
    [:a {:href (str "/" s) :class (c [:text :blue-600])}
     (when-let [icon (resolve-icon ztx res)]
       (cond (= (:type icon) :img)
             [:img {:src (:img icon) :class (c [:h 4] :inline-block [:mr 1] {:border-radius "100%" :margin-bottom "1px"})}]
             (= (:type icon) :ico)
             [:i {:class (str (str/join " " (map name (:icon icon))) " " (name (c [:mr 2] [:text :gray-500] :text-sm)))}]))
     (or (:title res) s)]
    [:a {:href (str "/" s) :class (c [:text :red-600] [:bg :red-100]) :title "Broken Link"} s]))

(defmethod inline-method :symbol-link
  [ztx m s]
  (symbol-link ztx s))

(defmethod inline-method :code
  [ztx m s]
  [:code {:class (c [:px 1.5] [:py 1] [:bg :gray-200]
                    :text-sm
                    {:border-radius "4px"
                     :font-family "ui-monospace,SFMono-Regular,SF Mono,Menlo,Consolas,Liberation Mono,monospace"})} s])

(defmethod inline-method :b
  [ztx m s]
  [:b s])


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
  [:div.code-block
   [:pre {:class (c :text-sm)
          :style {:position "relative" :white-space "pre-wrap"}}
    [:i.fas.fa-clipboard-list.copy-button
     {:title "Click to Copy"
      :style {:position  "relative"
              :float     "right"
              :top       "5px"
              :right     "20px"}}]
    [:code {:style {:word-wrap "break-word"} :class (str "language-" lang " hljs")} cnt]]])

(defmethod process-block :default [ztx tp args cnt]
  [:pre {:params args :tp tp}
   [:code.hljs cnt]])

(defmethod render-key :default [_ & _] nil)


(defmethod render-key
  [:summary]
  [ztx block]
  [:div {:class (c [:text :gray-600])}
   (render-content ztx block)])

(defn render-md [ztx data]
  (when data
    (zd.zentext/parse-block ztx data)))

(defmethod render-content :md
  [ztx {data :data}]
  [:div {:class (c [:px 0] [:py 1] [:bg :white] {:word-wrap "break"})}
   (zd.zentext/parse-block ztx data)])

(defmethod render-content :img
  [ztx {page :page {img :img} :annotations data :data}]
  (let [path (str/join "/" (butlast (str/split (:zd/file page) #"/")))
        src (cond
              (str/starts-with? data "http") data
              (str/starts-with? data "/") data
              :else (str path "/" data))]
    [:img (merge img {:src src})]))

(defmethod render-content :default
  [ztx {data :data :as block}]
  (cond
    (string? data) [:span (zd.zentext/parse-block ztx (str data))]
    (or (keyword? data) (boolean? data))
    [:span {:class (c [:text :green-600])} (str data)]
    ;; TODO: check link
    (nil? data) ""
    (symbol? data) (symbol-link ztx data)
    (number? data) (str data)
    (set? data) (conj (into [:div {:class (c :flex [:space-x 3] {:flex-wrap "wrap"})}
                             [:div {:class (c [:text :gray-500] :text-sm)} "#"]]
                            (mapv (fn [x] (render-content ztx {:data x})) data)))

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

(defmethod key-data :default [ztx path data] data)
(defmethod zd.methods/title-actions
  :override
  [ztx block]
  [:div])

(defmethod render-block :default
  [ztx {ann :annotations data :data path :path :as block}]
  [:div.zd-block {:class (str
                          " "
                          (when (:collapse ann) "zd-toggle")
                          " "
                          (when (get-in ann [:collapse :open]) "zd-open"))}
   (when-let [ann (:block ann)]
     (println :missed-render-block ann)
     [:div {:class (c [:text :red-800])}
      (str "Missed render-block for " ann)])
   [(keyword (str "h" (inc (count path)))) {:class (str "zd-block-title " (name (c :flex :items-baseline)))}
    [:div {:class (c :flex :flex-1)}
     (when (:collapse ann)
       [:i.fas.fa-chevron-right {:class (name (c [:mr 2] [:text :gray-500] :cursor-pointer
                                                 [:hover [:text :gray-600]]))}])
     (keypath path (or (:title ann) (let [k (last path)] (capitalize k))))]
    (zd.methods/title-actions ztx block)]

   [:div.zd-content (render-content ztx (update block :data (fn [d] (key-data ztx path d))))]])

(defmethod render-block :none [ztx block])

(defmethod render-block :badge
  [ztx {data :data path :path :as block}]
  [:div {:class (c :border [:m 1]  :inline-flex :rounded [:p 0])}
   [:div {:class (c :inline-block [:px 2] [:bg :gray-100] [:py 0.5] :text-sm [:text :gray-700] {:font-weight "400"})}
    (subs (str (last path)) 1)]
   [:div {:class (c [:px 2] [:py 0.5] :inline-block :text-sm)}
    (render-content ztx block)]])

(defmethod render-block :attribute
  [ztx {data :data path :path :as block}]
  [:div {:title "attribute" :class (c [:py 0.5] :flex :border-b :items-baseline [:space-x 4])}
   [:div {:class (c  [:text :gray-600] {:font-weight "500"})}
    (subs (str (last path)) 1)]
   [:div {:class (c)}
    (render-content ztx block)]])

(defmethod render-block :plain
  [ztx block]
  (render-content ztx block))

(defn table [ztx cfg data]
  (if-let [headers (or (:columns cfg)
                       (and (sequential? data) (map? (first data))
                            (keys (first data))))]
    [:table {:class (c :shadow-sm :rounded)
             :style {:display "block"
                     :overflow-x "overlay"}}
     [:thead
      (into [:tr] (->> headers (map-indexed (fn [i k] [:th {:class (c [:px 4] [:py 2] :border [:bg :gray-100]) :style (when (= i 0) {:width "200px" :word-wrap "break-word"})}
                                                       (capitalize (name k))]))))]
     (into [:tbody]
           (->> data
                (mapv (fn [x]
                        (into [:tr]
                              (->> headers
                                   (map-indexed (fn [i k]
                                                  [:td
                                                   {:class (c [:px 4] [:py 2] :border {:vertical-align "top"}) :style (when (= i 0) "white-space: break-word;")}
                                                   (render-content ztx {:data (get x k)})]))))))))]
    [:pre (pr-str data)]))

(defmethod render-content :table
  [ztx {ann :annotations data :data path :path :as block}]
  (table ztx (or (:table ann) {}) data))


(defmethod render-content :yaml
  [ztx {ann :annotations data :data path :path :as block}]
  [:div.code-block
   [:pre {:class (c :text-sm) :style {:white-space "pre-wrap"}}
    [:i.fas.fa-clipboard-list.copy-button
     {:title "Click to Copy"
      :style {:position  "relative"
              :float     "right"
              :top       "5px"
              :right     "20px"}}]
    [:code {:style {:word-wrap "break-word"} :class (str "language-yaml hljs")} (if (string? data) data (clj-yaml.core/generate-string data))]]])

(defmethod render-content :edn
  [ztx {ann :annotations data :data path :path :as block}]
  [:div.code-block
   [:pre {:class (c :text-sm) :style {:white-space "pre-wrap"}}
    [:i.fas.fa-clipboard-list.copy-button
     {:title "Click to Copy"
      :style {:position  "relative"
              :float     "right"
              :top       "5px"
              :right     "20px"}}]
    [:code {:style {:word-wrap "break-word"} :class (str "language-edn hljs")} (if (string? data) data (clj-yaml.core/generate-string data))]]])

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


(defmethod inline-function
  :resource
  [ztx m [sym & path]]
  (if-let [sym (zd.db/get-resource ztx sym)]
    (get-in sym path)
    [:div "Could not find " (pr-str sym)]))

(defmethod annotation :mermaid
  [nm params]
  {:content :mermaid :mermaid params})


(defmethod render-content :mermaid
  [ztx {{params :mermaid} :annotations data :data path :path}]
  [:div.mermaid data])

(defmethod render-content :mm
  [ztx {{params :mermaid} :annotations data :data path :path}]
  [:div.mermaid data])


(defmethod render-key
  [:zd/page]
  [_ {data :data :as block}]
  [:div
   [:h3 "Debug:Page"]
   [:pre {:class (c :text-sm)}
    [:code {:class (str "language-edn hljs")}
     (with-out-str (clojure.pprint/pprint (get-in block [:page :doc])))]]])

(defmethod render-key
  [:zd/resource]
  [_ {data :data :as block}]
  [:div
   [:h3 "Debug:Resource"]
   [:pre {:class (c :text-sm)}
    [:code {:class (str "language-edn hljs")}
     (with-out-str (clojure.pprint/pprint (get-in block [:page :resource])))]]])

(defmethod render-key
  [:tag]
  [ztx block]
  (let [references (zen.core/get-tag ztx (:data block))
        schemas    (->> (sort references)
                        (filter (comp #(clojure.string/starts-with? % "fhir.ru")
                                      namespace))
                        (mapv (fn [reference]
                                [reference (zen.core/get-symbol ztx reference)])))
        sorted-schemas (reverse (sort-by (comp :zendoc second) schemas))]
    [:div
     [:h3 (-> block :annotations :title)]
     [:ul
      (for [[reference schema] sorted-schemas]
        (let [zendoc (some-> schema :zendoc)]
          [:li
           (if zendoc
             [:a {:href  (str "/" zendoc)
                  :class (c [:text :blue-600])}
              (or
               (->> (zd.db/get-doc ztx (symbol zendoc))
                    (filter #(= [:title] (:path %)))
                    (first)
                    (:data))
               reference)]
             [:div {:class (c [:text :red-600])} (name reference)])]))]]))


(defmethod render-key [:menu-order] [_ _block] [:div])

(defmethod inline-method :mention
  [ztx m s]
  (symbol-link ztx (symbol (str "people." s))))

(defmethod inline-method :bold
  [ztx m s]
  [:b s])

(defmethod inline-method :italic
  [ztx m s]
  [:i s])

(defmethod inline-method :md/link
  [ztx m s]
  (let [[txt href] (str/split s #"\]\(" 2)]
    [:a {:href href :class (c [:text :blue-600])} txt]))

(defmethod inline-method :md/img
  [ztx m s]
  (let [[txt href] (str/split s #"\]\(" 2)]
    [:img {:src href :alt txt}]))

(defmethod render-content :markdown
  [ztx {data :data}]
  [:div {:class (c [:px 0] [:py 2] [:bg :white])}
   (markdown.core/md-to-html-string data)])


(defmethod render-key
  [:title]
  [_ {title :data :as block}]
  [:h1 {}
   (if-let [img (or (get-in block [:page :resource :avatar]) (get-in block [:page :resource :logo]))]
     [:img {:src img :class (c [:w 12] :inline-block [:mr 4] {:border-radius "100%"})}]
     (when-let [icon (get-in block [:page :resource :icon])]
       [:i {:class (str (str/join " " (map name icon))
                        " "
                        (name (c [:mr 2] [:text :gray-600])))}]))
   title])

(defmethod render-key [:avatar] [_ block] [:span])
(defmethod render-key [:menu-order] [_ block] [:span])
(defmethod render-key [:icon] [_ block] [:span])

(defmethod render-key
  [:toc]
  [_ {{doc :doc} :page :as block}]
  [:div {:class (c :text-sm)}
   (for [b doc]
     (when-not
      (or (contains? #{[:menu-order] [:title] [:avatar] [:toc]} (:path b))
          (get-in b [:annotations :block]))
       [:div {:class (c :flex [:space-x 2])}
        (for [_ (range (count (:path b)))]
          [:div {:class (c [:w 2])}])
        [:div {:class (c [:text :gray-400])} "●"]
        [:a {:href (str "#" (str/join (:path b))) :class (c [:text :blue-600])}
         (or (get-in b [:annotations :title])
             (capitalize (name (last (:path b)))))]]))])


(defmethod process-block "table" [ztx _ _ args]
  (let [[cols & rows] (->> (str/split-lines args) (mapv (fn [x] (str/split x #"\|"))))]
    [:table {:class (c :shadow-sm :rounded)}
     [:thead
      (into [:tr] (->> cols (mapv (fn [k] [:th {:class (c [:px 4] [:py 2] :border [:bg :gray-100])} k]))))]
     (into [:tbody]
           (->> rows
                (mapv (fn [x]
                        (into [:tr]
                              (->> x (mapv (fn [v] [:td {:class (c [:px 4] [:py 2] :border)} v]))))))))]))


(defn mindmap-stack [stack lvl]
  (loop [[[slvl idx] & is :as st] stack]
    (if (nil? slvl)
      (list [lvl (inc (or (second (first stack)) -1))])
      (cond
        (< slvl lvl)  (conj st [lvl 0])
        (= slvl lvl)  (conj is [lvl (inc idx)])
        (> slvl lvl)  (recur is)))))

(defn mindmap-assoc [res stack content]
  (let [path (->> stack
                  reverse
                  (mapcat (fn [[_ idx]] [:children idx])))]
    (assoc-in res path {:name content :children []})))

(defn parse-mindmap [txt]
  (let [[root & lns] (->> (str/split-lines txt) (remove str/blank?))]
    (loop [[l & lns] lns
           stack     (list)
           res       {:name (str/trim root) :children []}]
      (if (nil? l)
        res
        (if (str/blank? l)
          (recur lns stack res)
          (let [len (count l)
                data (str/replace l #"^\s*\*" "")
                lvl (- len (count data))
                stack' (mindmap-stack stack lvl)]
            (recur lns stack' (mindmap-assoc res stack' (str/replace data #"^\s*" "")))))))))

(defmethod render-content :mindmap
  [ztx {ann :annotations data :data path :path :as block}]
  (let [id (str (gensym))]
    [:div
     [:svg.mindmap {:id id :width "912" :height "600" :margin "0px -30px"}]
     [:script (str "mindmap('#" id "', " (cheshire.core/generate-string (parse-mindmap data)) ");")]]))
