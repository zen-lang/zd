(ns zd.html)

(comment)

(defn layout [ztx & cnt]
  [:html
   [:head [:style "body {padding: 1rem; background: #f9f9fb;}" ]]
   [:body
    [:table
     [:tr
      [:td {:style "vertical-align: top;"}
       [:div
        (for [[id r] (->> (:kg/resources @ztx)
                          (sort-by first))]
          (let [parts  (str/split (str id) #"\.")]
            [:a {:href (str id ".html") :style (str "display: block; margin-left: " (count parts) "rem;")}
             (last parts)])
          )]]
      (into [:td {:style "vertical-align: top; padding: 2rem;"}] cnt)]]]])

(defn render-site [ztx]
  (let [rs (:kg/resources @ztx)]
    (doseq [[id r] rs]
      (println :render id)
      (spit (str "/tmp/kg/" (str id) ".html")
            (hiccup.core/html
             (layout ztx
                     [:h1 (:kg/title r)]
                     [:table
                      (->> (dissoc r :kg/md)
                           (mapv (fn [[k v]]
                                   [:tr
                                    [:td [:b (str k)]]
                                    [:td (cond (symbol? v) [:a {:href (str v ".html")} (str v)]
                                               (string? v) v
                                               :else (pr-str v))]]))
                           (into [:tbody]))]
                     [:hr]
                     [:div (hiccup.util/as-str (markdown.core/md-to-html-string (:kg/md r)))]
                     [:div
                      [:h3 "Back refs"]
                      (->> rs
                           (mapcat (fn [[rid res]]
                                     (->> res
                                          (mapv (fn [[k v]] (when (= v id) [k rid])))
                                          (remove nil?))))
                           (group-by first)
                           (mapv (fn [[tp rss]]
                                   [:tr
                                    [:td
                                     (->> rss
                                          (mapv (fn [[_ x]] [:li [:a {:href (str x ".html")} (str x)]]))
                                          (into [:ul]))]
                                    [:td {:style "vertical-align: top;"} [:b (pr-str tp)]]]))
                           (into [:table])
                           )])))))

  (spit (str "/tmp/kg/index.html")
        (hiccup.core/html
         (layout ztx
                 [:ul
                  (for [[id r] (:kg/resources @ztx)]
                    [:li [:a {:href (str id ".html")} (str id)]])]))))

(defonce stop (atom false))


(comment

  (reset! stop true)

  (future
    (doseq [i (range 10000)]
      (when @stop (throw (Exception. "stop")))

      (Thread/sleep 5000)
      (do
        (def ztx (zen/new-context))
        (start ztx {:kg/path "/Users/niquola/kg"})
        (render-site ztx))

      ))
  )
