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

        plus-btn
        [:a {:class (c :cursor-pointer
                       [:text :gray-600]
                       [:ml 4]
                       [:hover [:text :green-600]])
             :href (str docname "." "_draft/edit")}
         [:i.fas.fa-plus]]

        container
        [:div {:class (c :flex :items-center :border-l [:ml 4])}]]

    (conj container edit-btn del-btn plus-btn)))

(defn breadcrumbs [ztx {{uri :uri} :request root :root} {{:keys [docname]} :zd/meta :as doc}]
  (let [parts (str/split (str docname) #"\.")
        icon-class (c :cursor-pointer [:text :gray-500] [:hover [:text :orange-600]])]
    [:div {:class (c :flex [:py 2])}
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
                  :class (c [:text "#4B5BA0"])}
              (last pth)]
             (when-not (= x (count parts))
               [:span {:class (c [:text :gray-500] [:mx 1.5] {:font-size "18px"})}
                "/"])]))])]))

(defn topbar [ztx ctx doc]
  [:div {:class (c :flex :items-center)}
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
  [:div {:class (c [:w "60vw"])}
   ;; TODO render errors in doc view
   (when-let [errs (seq (:errors m))]
     (methods/renderkey ztx ctx {:data errs :ann {} :key :zd/errors}))
   (doall
    (for [k (distinct (filter #(get doc %) (:doc m)))]
      (let [block {:data (get doc k)
                   :key k
                   :ann (assoc (get-in doc [:zd/meta :ann k]) :zd/render-subdoc? render-subdoc?)}]
        (render-key ztx ctx block))))
   (let [links (seq (get-in doc [:zd/meta :backlinks]))]
     (when-not render-subdoc?
       (methods/renderkey ztx ctx {:data links :key :zd/backlinks})))
   (when-let [subdocs (seq (filter #(get subs %) (:doc m)))]
     [:div {:class (c [:py 4])}
      (doall
       (for [sub-key (distinct subdocs)]
         [:div {:class (c [:my 2])}
          [:div {:class (c :flex :flex-row :border-b)}
           [:a {:id (str "subdocs-" (name sub-key))}
            #_[:span {:class (c [:text :gray-600])} "&"]
            [:span {:class (c :uppercase {:font-weight "600"})} (name sub-key)]]]
          (render-blocks ztx ctx (get-in doc [:zd/subdocs sub-key]) true)]))])])

(defn contents-sidebar [ztx {r :root :as ctx} {{order :doc anns :ann :as m} :zd/meta
                                               links :zd/backlinks subs :zd/subdocs :as doc}]
  (let [dockeys
        ;; TODO fix case when subdocs and dockey are the same
        (->> order
             (filter (fn [k]
                       (let [ka (get anns k)]
                         (and (get doc k)
                            ;; TODO exclude based on the metadata property like .sidebar false
                              (not (get ka :link-badge
                                        (get ka :badge
                                             (get ka :none))))))))

             (map name))
        ;; TODO move backlinks processing to memstore
        doclinks (->> (:backlinks m)
                      (map (fn [{d :doc}]
                             (if (str/includes? (str d) ".")
                               (str/join "." (butlast (str/split (str d) #"\.")))
                               r)))
                      (set)
                      (sort-by identity))
        subdocs (->> order
                     (filter #(get subs %))
                     (distinct))

        root (c :text-sm [:p 6] [:bg "white"])
        col  (c :flex :flex-col [:py 2])
        head (c :uppercase [:py 1])]
    [:div {:class root}
     (when (seq dockeys)
       [:div {:class col}
        [:div {:class head} "document"]
        ;; TODO make items clickable
        (for [k dockeys]
          [:a {:href (str "#" k)} k])])
     (when (seq subdocs)
       [:div {:class col}
        [:div {:class head} "subdocs"]
        (for [k subdocs]
          ;; TODO think about better convention?
          [:a {:href (str "#subdocs-" (name k))} k])])
     (when (seq doclinks)
       [:div {:class col}
        [:div {:class head} "backlinks"]
        (for [k doclinks]
          [:a {:href (str "#backlinks-" k)} k])])]))

(defn render-doc [ztx ctx doc]
  [:div
   (topbar ztx ctx doc)
   [:div#blocks {:class (c :flex :flex-row [:w-max "60rem"])}
    (render-blocks ztx ctx doc)
    (contents-sidebar ztx ctx doc)]])

(defn navigation [ztx {{{search-text :search} :query-params :as req} :request r :root :as ctx} doc]
  [:div#left-nav {:class (c :border-r
                            [:bg "#fbfbfb"]
                            [:h "100vh"]
                            :overflow-y-auto
                            :flex
                            :flex-col
                            :flex-grow
                            [:flex-shrink 0]
                            {:flex-basis "16rem"})}

   [:div {:class (c :flex :flex-row :items-baseline)}
    #_[:span {:class (c :relative [:right -5])}
       [:i.fas.fa-regular.fa-search]]
    [:input#zd-search-input
     {:type "search"
      :value search-text
      :class (c :border
                :outline-none
                [:w "100%"]
                [:rounded 4]
                :text-base
                [:m 6]
                [:px 2]
                [:py 1])}]]
   (if (not (str/blank? search-text))
     (let [query-result (map first (db/search ztx search-text #_(get-in doc [:zd/meta :docname]) #_page-number))]
       (if (seq query-result)
         [:div {:class (c :flex :flex-col [:pt 0] [:pr 6] [:pb 6] [:pl 6] {:flex-basis "18rem"})}
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
                   [:span {:class (c [:text :orange-500] [:py 1] [:px 2])}
                    "_schema"])]]]))]
         [:span {:class (c [:pt 0] [:pr 6] [:pb 6] [:pl 6] )}
          "No results"]))
     (let [grouped
           (->> (db/navbar-docs ztx)
                :docs
                (group-by (comp :section #(nth % 2)))
                (sort-by first)
                (reverse))]
       [:div#zd-menu
        {:class (c [:pt 0] [:pr 6] [:pb 6] [:pl 6] [:pseudo ">div:last-child>#section-break" :hidden])}
        (for [[section docs] grouped]
          [:div
           (for [[d] docs]
             (let [doc (memstore/get-doc ztx (symbol d))]
               [:div {:class (c :flex [:py 1.5] :items-center
                                :flex-row [:pseudo ":hover>a:last-child" :block] :justify-between)}
                [:a {:href (str "/" d)
                     :class (c [:w "100%"] :flex :flex-row :items-baseline)}
                 [:div {:class (c [:text :gray-700] [:w 6])}
                  (link/icon ztx doc)]
                 [:span {:class (c :capitalize [:pl 1])} d]]
                [:a {:class (c :cursor-pointer :text-lg [:text :gray-500] :hidden [:hover [:text :green-600]])
                     :href (str d "." "_draft/edit")}
                 [:i.fas.fa-plus]]]))
           [:div#section-break {:class (c :border-b [:my 2])}]])]))])

(defn doc-view [ztx ctx doc]
  [:div {:class (c :flex [:h "100%"])}
   (navigation ztx ctx doc)
   [:div#page {:class (c :flex :flex-grow [:flex-shrink 1] [:py 6] [:px 12] :overflow-y-auto
                         {:flex-basis "100%"})}
    (render-doc ztx ctx doc)]])

(def default-tpl ":title \"\"")

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

        anns (meta/annotations ztx)
        keypaths (->> (:zd/keys @ztx)
                      (mapv (fn [x] {:name (str x)})))
        zendoc {:text text
                :symbols symbols
                :keys keypaths
                :icons  icons/icons
                ;; TODO add completion from blocks meta
                :annotations anns
                :preview (-> (preview ztx ctx text) (hiccup/html))
                :doc (:docname m)}]
    [:script "var zendoc=" (json/generate-string zendoc)]))
