(ns zd.render
  (:require
   [zd.db :as db]
   [zd.schema :as schema]
   [zd.reader :as reader]
   [cheshire.core :as json]
   [hiccup.core :as hiccup]
   [clojure.java.io :as io]
   [zd.icons :as icons]
   [clojure.string :as str]
   [zd.blocks]
   [zd.methods :as methods]
   [stylo.core :refer [c]]
   [zd.loader :as loader]))

(defn tab? [s]
  (str/includes? (str s) "tab=folder"))

(defn tabs [ztx {{uri :uri qs :query-string} :request} doc]
  ;; TODO add url lib to zen web
  ;; TODO think about better approach to tab positioning
  [:div {:class (c [:mb "-28px"])}
   [:a#document-tab
    {:href uri
     :class (c [:py 4]
               [:px 6]
               [:rounded-t 6]
               [:mr 1]
               [:text :gray-600]
               :cursor-pointer
               :border-t :border-l :border-r)
     :style {:background-color (if (tab? qs) "#F7FAFC" "white")}}
    [:span {:class (c [:text :orange-500]
                      [:hover [:text :orange-600]]
                      [:pr 1.5])}
     [:i.fas.fa-clipboard]]
    [:span "document"]]

   [:a#folder-tab
    {:href (str uri "?tab=folder")
     :class (c [:py 4]
               [:px 6]
               [:rounded-t 6]
               :cursor-pointer
               [:text :gray-600]
               :border-t :border-l :border-r)
     :style {:background-color (if (tab? qs) "white" "#F7FAFC")}}
    [:span {:class (c [:text :orange-500]
                      [:hover [:text :orange-600]]
                      [:pr 1.5])}
     [:span.fas.fa-regular.fa-folder]]
    [:span "folder"]]])

(defn actions [ztx {{qs :query-string :as req} :request :as ctx} {{:keys [docname]} :zd/meta :as doc}]
  (let [edit-btn
        [:a {:class (c [:mx 4] [:text :gray-600] [:hover [:text :green-600]])
             :href (str docname "/" "edit" "?" qs)}
         [:i.fas.fa-edit]]

        create-btn
        [:a {:class (c [:mx 4] [:text :gray-600] [:hover [:text :green-600]])
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
        [:a {:class (c [:mx 4] [:text :gray-600] [:hover [:text :red-600]]
                       {:font-size "1.1rem"})
             :href ""
             :onclick (when-not (= docname 'index) del-script)}
         [:i.fas.fa-trash-xmark]]]
    [:div {:class (c :flex :items-center)
           :style {:visibility (if (and (some? qs) (str/includes? qs "tab=folder"))
                                 "hidden"
                                 "visible")}}
     edit-btn create-btn del-btn]))

(defn breadcrumbs [ztx {{uri :uri {root :root} :zd/config} :request} {{:keys [docname]} :zd/meta :as doc}]
  (let [parts (str/split (str docname) #"\.")
        icon-class (c :cursor-pointer [:text :orange-500] [:hover [:text :orange-600]])]
    (if (= (symbol root) docname)
      [:div {:class (c [:py 1])}
       [:a {:href (str "/") :class icon-class}
        [:span.fa-regular.fa-house]]]
      (->> (range 1 (+ 1 (count parts)))
           (mapcat (fn [x]
                     (let [pth (into [] (take x parts))
                           nm  (str/join "." pth)]
                       [[:a {:data-dir nm
                             :href (str "/" nm)
                             :class (c :cursor-pointer [:text :orange-500] [:hover [:text :orange-600]])}
                         (if (db/has-children? ztx nm)
                           [:span.fa-solid.fa-folder]
                           [:span.fa-regular.fa-file])]
                        [:a {:href (str "/" nm)
                             :class (c [:text :blue-500] [:px 2])}
                         (last pth)]
                        (when-not (= x (count parts))
                          [:span {:class (c [:text :gray-500] [:mr 3] {:font-size "18px"})}
                           "/"])])))
           (into [:div
                  [:a {:href (str "/") :class icon-class}
                   [:span.fa-regular.fa-house]
                   [:span {:class (c [:text :gray-500] [:m 3] {:font-size "18px"})}
                    "/"]]])))))

(defn topbar [ztx ctx doc]
  (-> [:div {:class (c :flex :flex-1 :items-center :justify-between :border-b [:bg "#F7FAFC"]
                       [:px 10] [:py 6])}]
        ;; TODO add transitions to tabs
      (conj (tabs ztx ctx doc))
      (conj (breadcrumbs ztx ctx doc))
      (conj (actions ztx ctx doc))))

(defn navigation [ztx ctx doc]
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
    [:i.fa-solid.fa-magnifying-glass]
    [:span "[ctrl-k]"]]
   [:aside#aside
    {:class (c [:text :gray-600] [:px 0] [:py 0]  :text-sm {})}]])

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
  [:div {:class (c [:pt 4])}
   (when-let [errs (:errors m)]
     (methods/renderkey ztx ctx {:data errs :ann {} :key :zd/errors}))
   (doall
    (for [k (filter #(get doc %) (:doc m))]
      (let [block {:data (get doc k)
                   :key k
                   :ann (get-in doc [:zd/meta :ann k])}]
        (render-key ztx ctx block))))
   (when-let [subdocs (not-empty (filter #(get subs %) (:doc m)))]
     [:div.zd-block
      [:h2 {:class (str "zd-block-title " (name (c :flex :items-baseline)))}
       "Subdocs"]
      (doall
       (for [sub-key subdocs]
         [:div {:class (c :border [:my 4] [:mr 2] :rounded)}
          [:div {:class (c [:bg :gray-100] [:px 8] [:py 2] :text-l [:text :gray-700]
                           {:font-weight "400"})}
           (name sub-key)]
          [:div {:class (c [:px 8] [:py 2])}
           (render-blocks ztx ctx (get-in doc [:zd/subdocs sub-key]))]]))])])

(defn render-folder [ztx ctx {{dn :docname} :zd/meta :as doc}]
  [:div#folder-items
   [:div.widget {:data-url (str "/" dn "/widgets/folder")}]])

(defn render-doc [ztx {{qs :query-string} :request :as ctx} doc]
  [:div {:class (c :flex :flex-1)}
   [:div {:class (c :flex-1 {:min-width "30em"})}
    (topbar ztx ctx doc)
    [:div {:class (c [:mr 12] :flex-1 [:pl 10] #_[:pt 6] [:pb 6])}
     (when-not (tab? qs)
       [:div#blocks {:class (c [:bg :white] {:color "#3b454e"})}
        (render-blocks ztx ctx doc)])
     (when (tab? qs)
       (render-folder ztx ctx doc))]]
   (when-let [links (seq (get-in doc [:zd/meta :backlinks]))]
     [:div {:class (c [:bg :gray-100] [:p 6] :border-l
                      {:height "100vh"
                       :overflow-y "auto"
                       :min-width "15em"
                       :max-width "20em"})}
      (methods/renderkey ztx ctx {:data links :key :zd/backlinks})])])

(defn *doc-view [ztx ctx doc]
  [:div {:class (c :flex :items-top :w-full)}
   #_(navigation ztx ctx doc)
   [:div#page {:class (c :flex-1 {:height "100vh" :overflow-y "auto"})}
    (render-doc ztx ctx doc)]])

(defn doc-view [ztx ctx config doc]
  ;; TODO move to layout mw
  (if (get-in ctx [:request :headers "x-body"])
    (render-doc ztx ctx doc)
    (*doc-view ztx ctx doc)))

(def default-tpl ":title \"\"\n:tags #{}")

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
         (loader/append-meta ztx)
         (schema/validate-doc ztx)
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
