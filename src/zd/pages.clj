(ns zd.pages
  (:require
   [zd.zentext]
   [zd.db]
   [zd.methods]
   [zd.parse]
   [zd.impl]
   [hiccup.core :as hiccup]
   [hiccup.page]
   [hiccup.util]
   [markdown.core]
   [clojure.string :as str]
   [stylo.core :refer [c]]
   [stylo.rule :refer [rule]]
   [garden.core]
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [zd.icons]))

(defmethod rule :grid-template-areas
  [_ & xs]
  [[:& {:grid-template-areas (apply pr-str xs)}]])

(defn breadcrumb [ztx name page]
  (let [parts (str/split (str name) #"\.")
        layout
        (->> (range (count parts))
             (mapcat (fn [x]
                       (let [pth (into [] (take (inc x) parts))
                             nm  (str/join "." pth)]
                         [[:a {:href (str "/" nm)
                               :class (c [:text :blue-500] [:px 2])}
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

        create-btn
        [:a {:class (c [:mx 4] [:text :green-600] [:hover [:text :green-700]])
             :href (cond->> (str "_draft/edit?" (get-in page [:request :query-string]))
                     (not= name 'index) (str name "."))}
         [:i.fas.fa-plus]]

        templ-btn
        [:a {:class (c [:mx 4] [:text :gray-600] [:hover [:text :gray-700]])
             :href (cond->> (str "_template/edit?" (get-in page [:request :query-string]))
                     (not= name 'index) (str name "."))}
         [:span "Template"]]

        del-script
        (format "fetch('/%s', {method: 'DELETE'}).then((resp)=> {
                  resp.text().then((docid) => {window.location.href = docid})})"
                name)

        del-btn
        (when-not (= name 'index)
          [:a {:class (c [:mx 4] [:text :red-600] [:hover [:text :red-700]]
                         {:font-size "1.1rem"})
               :href ""
               :onclick del-script}
           [:i.fas.fa-xmark]])]

    (conj layout edit-btn create-btn templ-btn del-btn)))

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
    [:i.fa-solid.fa-magnifying-glass]
    [:span "[ctrl-k]"]]
   [:aside#aside
    {:class (c [:text :gray-600] [:px 0] [:py 0]  :text-sm {})}]])

(defn generate-page [ztx doc]
  [:div {:class (c :flex :items-top :w-full)}
   (navigation ztx doc)
   [:div#page {:class (c :flex-1 {:height "100vh" :overflow-y "auto"})}
    (page ztx doc)]])

(defn find-template [ztx nm]
  (when nm
    (let [parts (str/split (str nm) #"\.")]
      (loop [parts parts]
        (when (not (empty? parts))
          (let [f (str "docs/" (str/join "/" (butlast parts)) "/_template.zd")]
            (if (.exists (io/file f))
              (slurp f)
              (recur (butlast parts)))))))))

(defn render-page [ztx config doc]
  (if (get-in doc [:request :headers "x-body"])
    (page ztx doc)
    (generate-page ztx doc)))

(defn preview [ztx text page]
  (page-content ztx (merge page (zd.parse/parse ztx text))))

(def default-tpl ":title \"\"\n:tags #{}")

(defn editor [ztx doc]
  (let [header
        (str ":zd/docname " (:zd/name doc) "\n")

        text (str header
                  (if (:zd/path doc)
                    (slurp (:zd/path doc))
                    (or (find-template ztx (:zd/name doc))
                        default-tpl)))
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
                :icons  zd.icons/icons
                :annotations (->> (keys (methods zd.methods/annotation))
                                  (mapv (fn [x]
                                          {:name (str "^" (name x))
                                           :title (name x)})))
                :preview (hiccup/html (preview ztx text doc))
                :doc (:zd/name doc)}]
    [:script "var zendoc=" (cheshire.core/generate-string zendoc)]))
