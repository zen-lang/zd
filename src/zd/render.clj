(ns zd.render
  (:require
   [clojure.string :as str]
   [zd.blocks]
   [zd.methods :as methods]
   [stylo.core :refer [c]]))

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

(defn get-block [ztx doc key]
  {:data (get doc key)
   :key key
   :ann (get-in doc [:zd/meta :ann key])})

(defn render-blocks [ztx ctx doc]
  [:div {:class (c [:pt 4])}
   (for [key (get-in doc [:zd/meta :doc])]
     (when (get doc key)
       (methods/renderkey ztx ctx (get-block ztx doc key))))])

(defn render-doc [ztx ctx doc]
  [:div {:class (c :flex :flex-1)}
   [:div {:class (c [:mr 12] :flex-1 {:min-width "30em"} [:pl 10] [:pt 6] [:pb 6])}
    (breadcrumbs ztx ctx doc)
    [:div {:class (c [:bg :white] {:color "#3b454e"})}
     (render-blocks ztx ctx doc)]]
   (if-let [links (->> [:zd/backlinks :zd/invalid-links]
                       (select-keys doc)
                       (map (fn [[k v]] [k v]))
                       (not-empty))]
     [:div {:class (c [:bg :gray-100] [:p 6] :border-l
                      {:height "100vh"
                       :overflow-y "auto"
                       :min-width "15em"
                       :max-width "35em"})}
      (for [[key _] links]
        (methods/renderkey ztx ctx (get-block ztx doc key)))])])

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
