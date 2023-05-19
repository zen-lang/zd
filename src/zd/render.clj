(ns zd.render
  (:require
   [zd.link :as link]
   [zd.meta :as meta]
   [zd.reader :as reader]
   [cheshire.core :as json]
   [hiccup.core :as hiccup]
   [zd.blocks.widgets :as widgets]
   [clojure.java.io :as io]
   [zd.icons :as icons]
   [clojure.string :as str]
   [zd.blocks]
   [zd.methods :as methods]
   [stylo.core :refer [c]]
   [zd.memstore :as memstore]
   [zd.db :as db]))

(defn actions [ztx {{uri :uri qs :query-string :as req} :request :as ctx} {{:keys [docname]} :zd/meta :as doc}]
  (let [edit-btn
        [:a {:class (c [:text :gray-600] [:hover [:text :green-600]] [:ml 4])
             :href (str docname "/" "edit" "?" qs)}
         [:i.fas.fa-edit]]

        ;; TODO move to .js
        del-script
        (format "if (confirm(\"delete document?\") == true){
                  fetch('/%s', {method: 'DELETE'}).then((resp)=> {
                  resp.text().then((docid) => {window.location.href = docid})})}"
                docname)

        del-btn
        [:a {:class (c [:text :gray-600] [:hover [:text :red-600]] [:ml 4])
             :href ""
             :onclick (when-not (= docname 'index) del-script)}
         [:i.fas.fa-trash-xmark]]

        container
        [:div {:class (c :flex :items-center :border-l [:ml 4])}]]

    (conj container edit-btn del-btn)))

(defn breadcrumbs [ztx {{uri :uri} :request root :root} {{:keys [docname]} :zd/meta :as doc}]
  (let [parts (str/split (str docname) #"\.")
        icon-class (c :cursor-pointer [:text :gray-500] [:hover [:text :orange-600]])]
    (if (= (symbol root) docname)
      [:a {:href (str "/") :class icon-class}
       [:span.fa-regular.fa-house]]
      [:div {:class (c :flex :flex-flow :items-baseline)}
       [:a {:href (str "/") :class icon-class}
        [:span.fa-regular.fa-house]]
       [:span {:class (c [:mx 1.5] [:text :gray-500])}
        "/"]
       (for [x (range 1 (+ 1 (count parts)))]
         (let [pth (into [] (take x parts))
               nm  (str/join "." pth)]
           [:div {:class (c :flex :flex-row :items-baseline)}
            (link/icon ztx (memstore/get-doc ztx (symbol nm)))
            [:a {:href (str "/" nm)
                 :class (c [:text :blue-500])}
             (last pth)]
            (when-not (= x (count parts))
              [:span {:class (c [:text :gray-500] [:mx 1.5] {:font-size "18px"})}
               "/"])]))])))

(defn search [ztx {{{search-text :search} :query-params :as req} :request r :root :as ctx} doc]
  [:div#zd-search {:class (c [:text :gray-600] [:px 6] [:py 4] [:h "100vh"] :overflow-y-auto)}
   [:div {:class (c [:w "14rem"] :flex :flex-row :items-baseline)}
    [:span {:class (c {:font-size "14px"
                       :margin-right "4px"
                       :color "#718096"})}
     [:i.fas.fa-regular.fa-search]]
    [:input#zd-search-input
     {:type "search"
      :value search-text
      :class (c :border
                [:text :gray-600]
                :text-sm
                [:rounded 14]
                [:py 0.5]
                [:px 3]
                [:w "100%"])}]]
   (when-let [search-text (not-empty search-text)]
     (let [query-result (map first (db/search ztx search-text #_(get-in doc [:zd/meta :docname]) #_page-number))]
       (if (seq query-result)
         [:div {:class (c [:py 2])}
          (for [[i docname] (map-indexed vector query-result)]
            (let [{{anns :ann lu :last-updated} :zd/meta p :parent :as doc}
                  (memstore/get-doc ztx (symbol docname))]
              [:div {:class (c [:py 2])}
               [:div {:class (c :overflow-hidden)}
                (link/symbol-link ztx docname)
                [:div {:class (c :flex :flex-row :items-baseline)}
                 (when (symbol? p)
                   [:div p])
                 (when (str/includes? (str docname) "_template")
                   [:span {:class (c [:text :orange-500] [:py 1] [:px 2])}
                    "_template"])
                 (when (str/includes? (str docname) "_schema")
                   [:span {:class (c [:text :orange-500] [:p 1] [:px 2])}
                    "_schema"])]]]))]
         [:span {:class (c [:text :gray-600] :text-sm)}
          "No results"])))])

(defn topbar [ztx ctx doc]
  [:div {:class (c :flex :items-baseline [:w "64rem"] [:py 4])}
   (breadcrumbs ztx ctx doc)
   (actions ztx ctx doc)])

(defn render-key [ztx ctx {k :key :as block}]
  (try (methods/renderkey ztx ctx block)
       (catch Exception e
         (let [err {:message (str "error rendering " (.getMessage e))
                    :path [k]
                    :type :zd/renderkey-error}
               err-block {:data [err] :key :zd/errors}]
                    ;; TODO add zen pub/sub event
           #_(clojure.pprint/pprint e)
           (println 'error-rendering-key k)
           (methods/renderkey ztx ctx err-block)))))

(defn render-blocks [ztx ctx {m :zd/meta subs :zd/subdocs :as doc} & [render-subdoc?]]
  [:div
   (when-let [errs (:errors m)]
     (methods/renderkey ztx ctx {:data errs :ann {} :key :zd/errors}))
   (doall
    (for [k (filter #(get doc %) (:doc m))]
      (let [block {:data (get doc k)
                   :key k
                   :ann (get-in doc [:zd/meta :ann k])}]
        (render-key ztx ctx block))))
   (when-let [subdocs (not-empty (filter #(get subs %) (:doc m)))]
     [:div {:class (c [:py 4])}
      (doall
       (for [sub-key subdocs]
         [:div {:class (c [:pt 4])}
          [:div {:class (c :border-b [:text :gray-700] :flex :flex-row)}
           [:span
            [:span {:class (c [:text :green-500])} "&"]
            [:span {:class (c [:text :gray-600])} (name sub-key)]]
           [:span {:class (c [:text :gray-500] [:pl 2])}
            "subdoc"]]
          (render-blocks ztx ctx (get-in doc [:zd/subdocs sub-key]) true)]))])
   (let [links (seq (get-in doc [:zd/meta :backlinks]))]
     (when-not render-subdoc?
       [:div (methods/renderkey ztx ctx {:data links :key :zd/backlinks})]))])

(defn render-doc [ztx {{qs :query-string} :request :as ctx} {{dn :docname} :zd/meta :as doc}]
  [:div {:class (c :flex :w-full :flex-col :flex-wrap :content-center)}
   (topbar ztx ctx doc)
   [:div#blocks {:class (c [:text "#3b454e"] [:pb 4] [:w "60rem"])}
    (render-blocks ztx ctx doc)]])

(defn navigation [ztx {{{search-text :search} :query-params :as req} :request r :root :as ctx} doc]
  (let [tab-class (c [:mr 2] [:px 3] [:py 0.2] :border [:rounded 14] [:hover :cursor-pointer [:text :orange-500]])]
    [:div#left-nav {:class (c [:text :gray-600] [:px 0] [:py 0]
                              :border-r
                              :fixed
                              :text-sm
                              [:top 0]
                              [:left 0]
                              [:w "18rem"]
                              [:h "100vh"]
                              {:overflow-y "auto"})}
     [:div {:class (c :flex :flex-col)}
      [:div {:class (c [:mt 4] [:px 8] :items-center :flex :flex-row)}
       [:span#zd-menu-tab {:class tab-class} "menu"]
       [:span#zd-search-tab {:class tab-class} "search"]]
      (let [{:keys [docs templates schemas views]} (db/navbar-docs ztx)]
        [:div#zd-menu {:class (c [:px 8] [:py 2])}
         (for [[d] docs]
           [:div {:class (c [:py 1])}
            (link/symbol-link ztx (symbol d))])])
      (search ztx ctx doc)]]))

(defn doc-view [ztx ctx doc]
  [:div {:class (c :flex :flex-row :justify-center)}
   (navigation ztx ctx doc)
   [:div#page
    (render-doc ztx ctx doc)]])

(def default-tpl ":title \"\"\n:tags #{}")

;; TODO find template in memstore
(defn find-template [ztx nm]
  (when nm
    (let [parts (str/split (str nm) #"\.")]
      (loop [parts parts]
        (when (seq parts)
          (let [f (str "docs/" (str/join "/" (butlast parts)) "/_template.zd")]
            (if (.exists (io/file f))
              (slurp f)
              (recur (butlast parts)))))))))

(defn preview [ztx ctx text]
  (let [parsed (reader/parse ztx ctx text)]
    (->> parsed
         (meta/append-meta ztx)
         (meta/validate-doc ztx)
         (render-blocks ztx ctx))))

(defn editor [ztx ctx {m :zd/meta :as doc}]
  (let [header (str ":zd/docname " (:docname m) "\n")
        text (str header
                  (if-let [pt (:path m)]
                    (slurp pt)
                    (or (find-template ztx (:docname m))
                        default-tpl)))
        symbols (->> (:zdb @ztx)
                     (mapv (fn [[k {ico :icon logo :logo tit :title}]]
                             {:title tit
                              :name k
                              :logo logo
                              :icon ico})))
        keypaths (->> (:zd/keys @ztx)
                      (mapv (fn [x] {:name x})))

        zendoc {:text text
                :symbols symbols
                :keys keypaths
                :icons  icons/icons
                ;; TODO add completion from blocks meta
                :annotations []
                :preview (-> (preview ztx ctx text) (hiccup/html))
                :doc (:docname m)}]
    [:script "var zendoc=" (json/generate-string zendoc)]))
