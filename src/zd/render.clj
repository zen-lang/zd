(ns zd.render
  (:require
   [zd.parser :as parser]
   [cheshire.core :as json]
   [hiccup.core :as hiccup]
   [clojure.java.io :as io]
   [zd.icons :as icons]
   [clojure.string :as str]
   [zd.blocks]
   [zd.methods :as methods]
   [stylo.core :refer [c]]
   [zd.loader :as loader]))

(defn breadcrumbs [ztx {{qs :query-string} :req} {{:keys [docname]} :zd/meta :as doc}]
  (let [parts (str/split (str docname) #"\.")
        layout
        (->> (range 1 (+ 1 (count parts)))
             (mapcat (fn [x]
                       (let [pth (into [] (take x parts))
                             nm  (str/join "." pth)]
                         [[:a {:data-dir nm
                               :href (str "/" nm)
                               :class (c :cursor-pointer [:text :orange-500] [:hover [:text :orange-600]])}
                           (if (= x (count parts))
                             [:span.fa-regular.fa-file]
                             [:span.fa-solid.fa-folder])]
                          [:a {:href (str "/" nm)
                               :class (c [:text :blue-500] [:px 2])}
                           (last pth)]
                          (when-not (= x (count parts))
                            [:span {:class (c [:text :gray-700] [:mr 3] {:font-size "18px"})}
                             "/"])])))
             (into [:div {:class (c :flex :flex-1 :items-center)}]))

        edit-btn
        [:a {:class (c [:mx 4] [:text :gray-600] [:hover [:text :green-600]])
             :href (str docname "/" "edit" "?" qs)}
         [:i.fas.fa-edit]]

        create-btn
        [:a {:class (c [:mx 4] [:text :gray-600] [:hover [:text :green-600]])
             :href (cond->> (str "_draft/edit?" qs)
                     (not= docname 'index) (str docname "."))}
         [:i.fas.fa-plus]]

        templ-btn
        [:a {:class (c [:mx 4] [:text :gray-600] [:hover [:text :gray-700]])
             :href (cond->> (str "_template/edit?" qs)
                     (not= docname 'index) (str docname "."))}
         [:span "Template"]]

        del-script
        (format "if (confirm(\"delete document?\") == true){
                  fetch('/%s', {method: 'DELETE'}).then((resp)=> {
                  resp.text().then((docid) => {window.location.href = docid})})}"
                docname)

        del-btn
        (when-not (= docname 'index)
          [:a {:class (c [:mx 4] [:text :gray-600] [:hover [:text :red-600]]
                         {:font-size "1.1rem"})
               :href ""
               :onclick del-script}
           [:i.fas.fa-xmark]])]

    (conj layout [:div {:class (c :flex :items-center {:margin-left "auto"})}
                  edit-btn create-btn templ-btn del-btn])))

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

(defn render-blocks [ztx ctx {m :zd/meta subs :zd/subdocs :as doc}]
  (let [[doc-keys subdocs]
        (partition-by #(empty? (get subs %)) (:doc m))]
    [:div {:class (c [:pt 4])}
     ;; TODO display zd/errors from document
     (for [k doc-keys]
       (let [block {:data (get doc k)
                    :key k
                    :ann (get-in doc [:zd/meta :ann k])}]
         (try (methods/renderkey ztx ctx block)
              (catch Exception e
                (let [err {:message (str "render " k " - " (.getMessage e))
                           :type :zd/renderkey-error}
                      err-block {:data [err] :key :zd/errors}]
                    ;; TODO add zen pub/sub event
                  (println 'error-rendering-key k)
                  (methods/renderkey ztx ctx err-block))))))
     (when (seq subdocs)
       [:div.zd-block
        [:h2 {:class (str "zd-block-title " (name (c :flex :items-baseline)))}
         "Subdocs"]
        (doall
         (for [sub-key subdocs]
           [:div {:class (c :border [:my 8] [:mr 2] :rounded)}
            [:div {:class (c [:bg :gray-100] [:px 4] [:py 2] :text-l [:text :gray-700]
                             {:font-weight "400"})}
             (name sub-key)]
            [:div {:class (c [:px 4])}
             (render-blocks ztx ctx (get-in doc [:zd/subdocs sub-key]))]]))])]))

(defn render-doc [ztx ctx doc]
  [:div {:class (c :flex :flex-1)}
   [:div {:class (c [:mr 12] :flex-1 {:min-width "30em"} [:pl 10] [:pt 6] [:pb 6])}
    (breadcrumbs ztx ctx doc)
    [:div {:class (c [:bg :white] {:color "#3b454e"})}
     (render-blocks ztx ctx doc)]]
   (when-let [links (seq (get-in doc [:zd/meta :backlinks]))]
     [:div {:class (c [:bg :gray-100] [:p 6] :border-l
                      {:height "100vh"
                       :overflow-y "auto"
                       :min-width "15em"
                       :max-width "20em"})}
      (methods/renderkey ztx ctx {:data links :key :zd/backlinks})])])

(defn *doc-view [ztx ctx doc]
  [:div {:class (c :flex :items-top :w-full)}
   (navigation ztx ctx doc)
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
  (->> (parser/parse ztx ctx text)
       (loader/append-meta ztx)
       (render-blocks ztx ctx)))

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
