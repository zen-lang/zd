(ns zd.blocks
  (:require
   [zd.db :as db]
   [zd.zentext.blocks]
   [zd.datalog :as d]
   [zd.loader :as loader]
   [zd.link :as link]
   [zd.zentext :as zentext]
   [clojure.pprint :as pprint]
   [stylo.core :refer [c]]
   [clojure.string :as str]
   [zd.methods :as methods]))

(defn keystr [key]
  (->> key
       (map (fn [k]
              (if (keyword? k)
                (name k)
                (str k))))
       (clojure.string/join ".")
       (str ".")))

(defmethod methods/renderkey :linkedin
  [ztx {{m :zd/meta} :doc} {data :data :as block}]
  (let [id (if (and data (not (str/blank? data)))
             data
             (last (str/split (str (:docname m)) #"\.")))]
    [:a {:class (str "badge " (name (c :border [:m 1]  :inline-flex :rounded
                                       [:py 1]
                                       :text-sm [:text :blue-500] [:px 2])))
         :href
         (if (str/starts-with? (str (:docname m)) "organizations.")
           (str "https://www.linkedin.com/company/" id "/")
           (str "https://www.linkedin.com/in/" id "/"))}
     [:i.fa-brands.fa-linkedin]]))

(defmethod methods/renderkey :zd/docname
  [ztx ctx {data :data :as block}]
  [:div {:class (str "badge " (name (c :border [:my 1] [:mr 2]  :inline-flex :rounded [:p 0])))}
   [:div {:class (c :inline-block [:px 2] [:bg :gray-100] [:py 0.5] :text-sm [:text :gray-700]
                    {:font-weight "400" :padding-top ".3rem"})}
    [:i.fas.fa-file]]
   [:div {:class (c [:px 2] [:py 0.5] :inline-block :text-sm)}
    data]])

(defmethod methods/renderkey :link-badge
  [ztx ctx {data :data k :key}]
  [:div {:class (c :border [:m 1]  :inline-flex :rounded [:p 0])}
   [:a {:href data
        :target "_blank"
        :class (c :inline-block [:px 2] [:bg :gray-100] [:py 0.5] :text-sm [:text :blue-600] {:font-weight "400"}
                  [:hover [:bg :gray-200]])}
    k]])

(defmethod methods/renderkey :title
  [ztx {doc :doc} {title :data :as block}]
  [:h1 {:class (c :flex :items-center)}
   (if-let [img (or (:avatar doc) (:logo doc))]
     [:img {:src img
            :class (c [:w 8] [:h 8] :inline-block [:mr 2] {:border-radius "100%"})}]
     (when-let [icon (:icon doc)]
       [:i {:class (str (str/join " " (map name icon))
                        " "
                        (name (c [:mr 2] [:text :gray-600])))}]))
   title])

(defmethod methods/renderkey :zd/backlinks
  [ztx ctx {:keys [data] :as block}]
  (let [links
        (->> data
             (map (fn [{d :doc p :path t :to}]
                    {:to t
                     :doc d
                     :path (->> (map name p)
                                (str/join ".")
                                (str ":"))}))

             (sort-by :doc)
             (group-by :path))]
    [:div {:class (c [:text :gray-600])}
     [:div {:class (c [:px 0])}
      [:div {:class (c :text-lg [:mb 4])}
       "Backlinks"]
      (doall
       (for [[p ls] links]
         [:div {:class (c [:mb 4])}
          [:div {:class (c :border-b [:py 2])} p]
          (for [{:keys [to doc path]} ls]
            [:div {:class (c [:pb 1]:text-sm)} (link/symbol-link ztx doc)])]))]]))

(defmethod methods/rendercontent :edn
  [ztx ctx {:keys [data] :as block}]
  (cond
    (or (string? data) (number? data)) (zd.zentext/parse-block ztx (str data) block)
    (symbol? data) (link/symbol-link ztx data)
    (keyword? data) (zd.zentext/parse-block ztx (str data) block)
    (or (set? data) (and (vector? data)
                         (every? symbol? data)))
    (->> data
         (mapv (fn [x] (methods/rendercontent ztx ctx (assoc block :data x))))
         (into [:div {:class (c :flex [:space-x 3] {:flex-wrap "wrap"})}
                [:div {:class (c [:text :gray-500] :text-sm)} "#"]]))
    :else
    [:div.code-block
     [:pre {:class (c :text-sm) :style {:white-space "pre-wrap"}}
      [:i.fas.fa-clipboard-list.copy-button
       {:title "Click to Copy"
        :style {:position  "relative"
                :float     "right"
                :top       "5px"
                :right     "20px"}}]
      [:code {:style {:word-wrap "break-word"} :class (str "language-edn hljs")}
       (if (string? data)
         data
         (with-out-str (pprint/pprint data)))]]]))

(defmethod methods/rendercontent :zentext
  [ztx ctx {:keys [data] :as block}]
  [:div {:class (c [:px 0] [:py 1] {:word-wrap "break"})}
   (zentext/parse-block ztx data block)])

(defmethod methods/renderkey :badge
  [ztx ctx {key :key :as block}]
  [:div {:class (str "badge " (name (c :border [:my 1] [:mr 2]  :inline-flex :rounded [:p 0])))}
   [:div {:class (c :inline-block [:px 2] [:bg :gray-100] [:py 0.5] :text-sm [:text :gray-700] {:font-weight "400"})}
    key]
   [:div {:class (c [:px 2] [:py 0.5] :inline-block :text-sm)}
    (methods/rendercontent ztx ctx block)]])

(defmethod methods/renderkey :attribute
  [ztx ctx {k :key :as block}]
  [:div {:title "attribute" :class (c [:py 0.5] :flex :items-center [:space-x 4])}
   [:div {:class (c  [:text :gray-600] {:font-weight "500"})}
    k]
   [:div {:class (c)}
    (methods/rendercontent ztx ctx block)]])

(defmethod methods/renderkey :none
  [ztx ctx block])

(defmethod methods/renderkey :hide
  [ztx ctx block])

(defmethod methods/render-cell :xt/id
  [ztx ctx key {:keys [xt/id]}]
  (let [res (loader/get-doc ztx (symbol id))]
    [:a {:href (str "/" id) :class (c :inline-flex [:px 2] [:py 1] :items-center [:text :blue-600] [:hover [:underline]])}
     (link/icon ztx res)
     (or (:title res) (symbol id))]))

(defmethod methods/render-cell :tags
  [ztx ctx key {:keys [tags]}]
  (when (not-empty tags)
    [:div {:class (c [:px 2] [:py 0.5] :text-sm)}
     (for [t tags]
       [:span {:class (c [:py 1] :block)} (link/symbol-link ztx t)])]))

(defmethod methods/render-cell :needs
  [ztx ctx key {:keys [needs]}]
  (let [arg {:data (if (string? needs) needs "")
             :ann {:zd/content-type :zentext}
             :key :customer-needs}]
    [:span {:class (c [:px 2] [:py 0.5])}
     (methods/rendercontent ztx ctx arg)]))

(defmethod methods/render-cell :rel
  [ztx ctx key row]
  [:div
   (for [r (:rel row)]
     [:span {:class (c :block [:py 1])}
      (when (string? r)
        (link/symbol-link ztx (symbol r)))])])

(defn table [ztx ctx headers data]
  [:table {:class (c :shadow-sm :rounded)
           :style {:display "block"
                   :overflow-x "overlay"}}
   [:thead
    (->> headers
         (map (fn [k]
                [:th {:class (c [:px 4] [:py 2] :border [:bg :gray-100])}
                 (str/lower-case (name k))]))
         (into [:tr]))]
   (->> data
        (sort-by (fn [r]
                   (if-let [v (get r (first headers))]
                     (if (string? v)
                       v
                       (pr-str v)))))
        (mapv (fn [row]
                [:tr
                 (doall
                  (for [h headers]
                    [:td {:class (c [:px 4] [:py 2] :border {:vertical-align "top"})}
                     (methods/render-cell ztx ctx h row)]))]))
        (into [:tbody]))])

(defmethod methods/rendercontent :datalog
  [ztx ctx {{headers :table-of} :ann data :data :as block}]
  (let [result (d/query ztx data)]
    (cond
      (seq headers) (table ztx ctx headers (map first result))
      ;; TODO think about this table predicate
      (and (set? result)
           (every? vector? result))
      (let [headers* (->> (map first result)
                          (mapcat keys)
                          (set)
                          (sort-by name))]
        (table ztx ctx headers* (map first result)))
      :else
      (methods/rendercontent ztx ctx {:data result
                                      :k (:key block)
                                      :ann {:zd/content-type :edn}}))))

(defmethod methods/renderkey :zd/errors
  [ztx ctx {errors :data k :key :as block}]
  (when (seq errors)
    [:div {:class (c [:text :red-700] [:p 4] [:my 4] :rounded
                     {:background-color "#fff5f5"})}
     [:ul {:class (c :font-bold :text-lg [:mb 2] [:ml 0] {:color "#e53e3e"})}
      "Document errors"]
     (for [err (sort-by :type errors)]
       [:li {:class (c [:mb 1] [:py 1] :flex [:space-x 3] [:text :gray-600])}
        [:span (->> (:path err)
                    (map (fn [p] (if (keyword? p) (name p) (str p))))
                    (str/join ".")
                    (str ":"))]
        [:span {:class (c [:ml 4] {:text-align "right"})} (:message err)]])]))

(defmethod methods/renderkey :table
  [ztx ctx {{headers :table} :annotations data :data}]
  (if (and (sequential? data) (every? map? data))
    (table ztx ctx (or headers (keys (first data))) data)
    [:pre (pr-str data)]))

(defn add-page-param [qs c]
  (let [qs (str qs)]
    (cond
      (str/includes? qs "page=") (str/replace qs #"page=\d+" (str "page=" c))
      (str/blank? qs) (str "?page=" c)
      :else (str qs "&page=" c))))

(defn pagination [req items-count]
  (let [pages-count (+ (quot items-count 24)
                       (if (= 0 (rem items-count 24))
                         0
                         1))
        path (get-in req [:headers "x-client-path"])
        qs (get-in req [:headers "x-client-qs"])]
    [:div {:class (c :flex :flex-row :justify-center [:py 4])}
     (for [pn (map #(+ 1 %)
                   (range pages-count))]
       [:a {:href (str path (add-page-param qs pn))
            :class (c [:mr 1.5] [:py 0.5] [:px 1.5] :text-sm [:text :gray-600]
                      :border :rounded
                      [:hover
                       [:cursor-pointer]
                       [:bg :gray-500]
                       [:text "white"]])}
        pn])]))

(defn docs-cards [ztx ctx summary-keys query-result]
  [:div
   (for [[i [docname]] (map-indexed vector query-result)]
     (let [{{anns :ann} :zd/meta :as doc} (loader/get-doc ztx (symbol docname))]
       [:div {:class (c [:py 8] [:px 2] [:bg "#F7FAFC"])
              :style {:background-color (if (even? i)
                                          "#F7FAFC"
                                          "white")}}
        [:div {:class (c [:pb 4] :text-lg)}
         (link/symbol-link ztx docname)]
        (when-let [desc (get doc :desc)]
          [:div {:class (c [:text :gray-700] [:pb 2])}
           (let [text-limit 128
                 desc-text
                 (->> (map-indexed vector (str desc))
                      (take-while (fn [[i ch]]
                                    (or (< i text-limit)
                                        (and (>= i text-limit)
                                             (not= ch \.)))))
                      (map second)
                      (apply str))
                 block {:key :desc :data desc-text :ann (get anns :desc)}]
             (methods/rendercontent ztx {} block))])
        [:div
         (doall
          (for [[k v] (select-keys doc summary-keys)]
            (methods/renderkey ztx {} {:key k :data v :ann (get anns k)})))]]))])

(defmethod methods/widget :folder
  [ztx {{config :zd/config :as req} :request :as ctx} {{dn :docname} :zd/meta :as doc}]
  (let [summary-keys
        (->> (get-in @ztx [:zd/schema])
             (filter (fn [[k v]]
                       (= :zd/summary (:group v))))
             (map first)
             (vec))
        dn-param (if (= (symbol (:root config)) dn)
                   ""
                   (str dn))
        page-number (->> (get-in req [:headers "x-client-qs"])
                         (re-matches #".*page=(\d+).*")
                         (second))
        query-result (db/children ztx dn-param page-number)
        items-count (if-let [c (ffirst (db/children-count ztx dn-param))]
                      c
                      0)]
    [:div
     (if (> items-count 0)
       [:div {:class (c :flex :justify-center [:pt 4] [:text :gray-600] :text-sm)}
        [:span "total: "] [:span items-count]]
       [:div {:class (c [:text :gray-600] [:my 2])}
        "Add a document with + button"])
     (when (> items-count 24)
       (pagination req items-count))
     (docs-cards ztx ctx summary-keys query-result)
     (when (> items-count 24)
       (pagination req items-count))]))
