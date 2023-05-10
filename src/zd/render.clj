(ns zd.render
  (:require
   [zd.link :as link]
   [zd.meta :as meta]
   [zd.reader :as reader]
   [cheshire.core :as json]
   [hiccup.core :as hiccup]
   [clojure.java.io :as io]
   [zd.icons :as icons]
   [clojure.string :as str]
   [zd.blocks]
   [zd.methods :as methods]
   [stylo.core :refer [c]]
   [zd.memstore :as memstore]
   [zd.db :as db]))

(defn tab? [s]
  (str/includes? (str s) "tab=folder"))

(defn actions [ztx {{uri :uri qs :query-string :as req} :request :as ctx} {{:keys [docname]} :zd/meta :as doc}]
  (let [edit-btn
        [:a {:class (c [:text :gray-600] [:hover [:text :green-600]] [:ml 4])
             :href (str docname "/" "edit" "?" qs)}
         [:i.fas.fa-edit]]

        create-btn
        [:a {:class (c [:text :gray-600] [:hover [:text :green-600]] [:ml 4])
             :href (cond->> (str "_draft/edit?" qs)
                     (not= docname 'index) (str docname "."))}
         [:i.fas.fa-plus]]

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

        folder-btn
        [:a#folder-tab
         {:href (str uri "?tab=folder")}
         [:span {:class (c [:text :gray-600])}
          [:i.fas.fa-regular.fa-folder]]]

        container
        [:div {:class (c :flex :items-center [:ml 4])}]]

    (if (tab? qs)
      (conj container folder-btn create-btn)
      (conj container folder-btn edit-btn del-btn))))

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
           [:div {:class (c :flex :flex-row :items-center)}
            (link/icon ztx (memstore/get-doc ztx (symbol nm)))
            [:a {:href (str "/" nm)
                 :class (c [:text :blue-500])}
             (last pth)]
            (when-not (= x (count parts))
              [:span {:class (c [:text :gray-500] [:mx 1.5] {:font-size "18px"})}
               "/"])]))])))

(defn search [ztx {{{search-text :search} :query-params :as req} :request} doc]
  [:div
   [:div {:class (c [:w "14rem"] :flex :flex-row :items-baseline)}
    [:span {:class (c {:font-size "14px"
                       :margin-right "4px"
                       :color "#718096"})}
     [:i.fas.fa-regular.fa-search]]
    [:input#zd-search
     {:type "search"
      :oninput "on_doc_search()"
      :value search-text
      :class (c :border
                [:text :gray-600]
                :text-sm
                :rounded
                [:py 0.5]
                [:px 2]
                [:w "100%"])}]]])

(defn topbar [ztx ctx doc]
  [:div {:class (c :flex :items-baseline :justify-between [:w "64rem"] [:py 4])}
   (breadcrumbs ztx ctx doc)
   (actions ztx ctx doc)
   (search ztx ctx doc)])

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

(defn render-blocks [ztx ctx {m :zd/meta subs :zd/subdocs :as doc}]
  [:div
   (when-let [errs (:errors m)]
     (methods/renderkey ztx ctx {:data errs :ann {} :key :zd/errors}))
   (doall
    (for [k (filter #(get doc %) (:doc m))]
      (let [block {:data (get doc k)
                   :key k
                   :ann (get-in doc [:zd/meta :ann k])}]
        (render-key ztx ctx block))))
   (when-let [links (seq (get-in doc [:zd/meta :backlinks]))]
     [:div (methods/renderkey ztx ctx {:data links :key :zd/backlinks})])
   (when-let [subdocs (not-empty (filter #(get subs %) (:doc m)))]
     [:div
      (doall
       (for [sub-key subdocs]
         [:div {:class (c [:pt 4])}
          [:div {:class (c :border-b [:text :gray-700] :flex :flex-row :justify-between)}
           [:span
            [:span {:class (c [:text :green-500])} "&"]
            [:span {:class (c [:text :gray-600])} (name sub-key)]]
           [:span {:class (c [:text :gray-500])}
            "subdoc"]]
          [:div {:class (c [:pl 4])}
           (render-blocks ztx ctx (get-in doc [:zd/subdocs sub-key]))]]))])])

(defn render-doc [ztx {{qs :query-string} :request :as ctx} {{dn :docname} :zd/meta :as doc}]
  [:div {:class (c :flex :w-full :flex-col :flex-wrap :content-center)}
   (topbar ztx ctx doc)
   (if (tab? qs)
     [:div#folder-items {:class (c [:w-max "64rem"] [:w-min "30rem"] [:py 4])}
      [:div.widget {:data-url (str "/" dn "/widgets/folder")}]]
     [:div#blocks {:class (c [:text "#3b454e"] [:pb 4] [:w-max "64rem"] [:w-min "30rem"])}
      (render-blocks ztx ctx doc)])])

(defn navigation [ztx ctx doc]
  [:div#left-nav {:class (c [:text :gray-600] [:px 0] [:py 0]
                            :text-sm
                            :border-r
                            :fixed
                            [:bg :gray-100]
                            {:height "100vh"
                             :overflow-y "auto"})}
   [:div {:class (c [:py 12] [:px 6])}
    (for [d (db/root-docs ztx)]
      [:div {:class (c [:py 0.5])}
       (link/symbol-link ztx (symbol d))])]])

(defn doc-view [ztx ctx doc]
  [:div
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
