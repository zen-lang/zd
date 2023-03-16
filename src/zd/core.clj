(ns zd.core
  (:require
   [zd.parse]
   [zd.layout]
   [hiccup.core :as hiccup]
   [zen-web.core :as web]
   [zd.methods :as meth]
   [zen.core :as zen]
   [zd.db :as db]
   [zd.datalog]
   [zd.pages :as pages]
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [ring.util.response]
   [clojure.string :as str]
   [gcp.storage])
  (:import [java.io InputStream]
           [java.nio.file Files CopyOption StandardCopyOption FileSystems]
           [java.io StringReader]
           [java.util Timer TimerTask]))

(defn debounce
  ([f] (debounce f 300))
  ([f timeout]
   (let [timer (Timer.)
         task (atom nil)]
     (with-meta
       (fn [& args]
         (when-let [t ^TimerTask @task]
           (.cancel t))
         (let [new-task (proxy [TimerTask] []
                          (run []
                            (apply f args)
                            (reset! task nil)
                            (.purge timer)))]
           (reset! task new-task)
           (.schedule timer new-task timeout)))
       {:task-atom task}))))

(defn reload-hard [ztx dirs]
  (swap! ztx dissoc :zdb)
  (swap! ztx assoc :zrefs {})
  (zen/pub ztx 'zd/reload {:dirs dirs})
  (db/load-dirs ztx dirs))

(defn reload [ztx config]
  (println :request-reload)
  (debounce #(reload-hard ztx (:paths config)))
  :ok)

(defmethod web/middleware-in 'zd/dev-reload
  [ztx _cfg {config :zendoc/config} & args]
  (reload ztx config))

(defmethod zen/op 'zd/render-page
  [ztx cfg {{id :id} :route-params
            config :zendoc/config
            page :page :as req} & opts]
  (cond
    (nil? page)
    {:status 301
     :headers {"Location" (str "/" id "/edit" "?" (:query-string req))
               "Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}}

    :else
    {:status 200
     :body (pages/render-page ztx config page)}))

(defmethod zen/op 'zd/render-editor
  [ztx _cfg {{id :id} :route-params page :page :as req} & opts]
  (let [page (or page {:zd/name (symbol id)})]
    {:status 200
     :body (zd.pages/editor ztx page)}))

(defmethod zen/op 'zd/update-zendoc
  [ztx _cfg {{id :id} :route-params :as req} & opts]
  (let [content (slurp (:body req))
        name "editable-res"]
    (db/load-content! ztx {:path "" :resource-path name :content content})
    (let [doc (db/get-page ztx (symbol name))]
      {:status 200
       :body (zd.pages/page-content ztx doc)})))

(defmethod zen/op 'zd/render-preview
  [ztx _ {{id :id} :route-params :as req} & opts]
  {:headers {"Content-Type" "text/html"}
   :body (->> {:zd/name id :name id :zd/file (str (str/replace id #"\." "/") ".zd")}
              (zd.pages/preview ztx (slurp (:body req)))
              (hiccup/html))
   :status 200})

(defn parse-doc [doc]
  (->> doc
       (StringReader.)
       (io/reader)
       (line-seq)
       (map (fn [s]
              (if (str/starts-with? s ":")
                (map #(apply str %)
                     (split-with #(not= \space %) s))
                s)))))

(defn unparse-doc [doc]
  (->> doc
       (map (fn [bl]
              (if (seq? bl)
                (apply str bl)
                bl)))
       (str/join "\n")))

(defn inferred-block? [block]
  (and (seq? block) (= (first block) ":zd/docname")))

(defmethod zen/op 'zd/delete-zendoc
  [ztx _cfg {pths :zd/paths {:keys [id]} :route-params :as req} & opts]
  (println :delete id)
  (let [parts (str/split id #"\.")
        filepath
        ;; TODO scan all paths?
        (str (first pths)
             "/"
             (str/join "/" parts)
             ".zd")

        redirect
        (if-let [parent (not-empty (butlast parts))]
          (str "/" (str/join "." parent))
          "/index")]
    (try (io/delete-file filepath)
         ;; TODO emit error event via zen pub sub
         (catch Exception e
           (clojure.pprint/pprint (.getMessage e))))
    ;; TODO load single document into db
    (reload-hard ztx pths)
    {:status 200 :body redirect}))

(defmethod zen/op 'zd/save-zendoc
  [ztx _cfg {pths :zd/paths {id :id} :route-params :as req} & opts]
  (println :save id)
  (let [content (slurp (:body req))
        parsed (parse-doc content)
        docname
        (->> parsed
             (filter inferred-block?)
             first second str/trim)
        content*
        (->> parsed
             (remove inferred-block?)
             (unparse-doc))]

    (cond (or (empty? docname)
              (str/ends-with? docname "."))
          ;; TODO add errors view
          {:status 422 :body "Add not empty :zd/docname"}

          (str/ends-with? docname "_draft")
          {:status 422 :body "Change :zd/docname from _draft"}

          :else
          ;; TODO prefix with :zd/paths from ztx
          (let [dirname
                (->> (str/split docname #"\.")
                     butlast
                     (str/join "/")
                     (str (first pths) "/"))

                filename (str (first pths) "/" (str/replace docname "." "/") ".zd")]

            #_(when-not (= id docname)
              (let [orig-filename (str (first pths) "/" (str/replace id "." "/") ".zd")]
                (try (io/delete-file orig-filename)
                   ;; TODO emit error event via zen pub sub
                     (catch Exception e
                       (clojure.pprint/pprint (.getMessage e))))))

            (.mkdirs (io/file dirname))
            (spit filename content*)
            ;; TODO load single document into db
            (reload-hard ztx pths)
            {:status 200 :body (str "/" docname)}))))

(defmethod zen/op 'zd/save-file
  [ztx _cfg {{id :id} :route-params :as req} & opts]
  (let [file-name (str (get-in req [:headers "file-name"]))
        dir-path (str "files/" id)
        file-path (str dir-path "/" file-name)
        file (io/file file-path)]
    (Files/copy ^java.io.InputStream (:body req)
                ^java.nio.file.Path (.toPath file)
                (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))
    {:body (json/generate-string {:file file-path
                                  :name file-name})
     :status 200}))

(defmethod zen/op 'zd/get-file
  [ztx _cfg {{id :id file-name :file} :route-params} & opts]
  (let [file-path (str "files/" id "/" file-name)
        file (io/file file-path)]
    (if (.exists file)
      (ring.util.response/file-response file-path)
      {:status 404})))

(defmethod web/middleware-in 'zd/resolve-page
  [ztx _cfg {{id :id} :route-params :as req} & opts]
  {:page (when-let [page (db/get-page ztx (symbol id))]
           (assoc page :request req))})

(defmethod web/middleware-in 'zd/snap-config
  [ztx cfg req & opts]
  (let [{pths :paths :as zendoc-cfg} (get-in @ztx [:zen/state :zendoc :state])]
    {:zd/config zendoc-cfg
     :zd/paths pths}))

(defmethod zen/op 'zd/render-widget
  [ztx _cfg {{id :id wgt :widget-id} :route-params :keys [page]} & opts]
  (if-not (nil? page)
    {:status 200
     :body (meth/widget ztx wgt page)}
    {:status 200
     :body [:div "Error: " id " is not found"]}))

(defmethod web/middleware-out 'zd/layout
  [ztx _cfg {page :page {lay-sym :layout} :zd/config :as req} {bdy :body :as resp} & args]
  (when (and (not (string? bdy)) (= 200 (:status resp)))
    {:headers {"Content-Type" "text/html"}
     :body (meth/layout ztx (zen/get-symbol ztx lay-sym) bdy page)}))

;; this should be done as db start
(defmethod zen/start 'zd/document-engine
  [ztx config & opts]
  (reload-hard ztx (:paths config))
  config)

(defmethod zen/stop 'zd/document-engine
  [ztx config & opts]
  ;; TODO dissoc zendoc state from memory
  )

