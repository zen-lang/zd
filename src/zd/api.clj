(ns zd.api
  (:require
   [zd.gitsync]
   [zd.meta :as meta]
   [zd.datalog]
   [hiccup.core :as hiccup]
   [zd.loader :as loader]
   [zd.reader :as reader]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [zen.core :as zen]
   [zd.methods :as methods]
   [zd.render :as render]
   [zd.layout]
   [zen-web.core :as web])
  (:import [java.io StringReader]))

(defmethod web/middleware-in 'zd/append-config
  [ztx {cfg-symbol :zendoc-config} req & opts]
  (let [{pths :paths :as zendoc-cfg} (zen/get-symbol ztx cfg-symbol)]
    {:zd/config zendoc-cfg
     :zd/config-symbol cfg-symbol
     :zd/paths pths}))

(defmethod web/middleware-in 'zd/append-doc
  [ztx _cfg {{id :id} :route-params :as req} & opts]
  (when (some? id)
    {:doc (loader/get-doc ztx (symbol id))}))

(defmethod zen/op 'zd/render-doc
  [ztx cfg {{id :id} :route-params
            config :zd/config
            sym :zd/config-symbol
            uri :uri
            hs :headers
            doc :doc :as req} & opts]
  (cond
    (= uri "/")
    (let [config (zen/get-symbol ztx sym)]
      {:status 301
       :headers {"Location" (str "/" (symbol (:root config)) "?" (:query-string req))
                 "Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}})

    (nil? doc)
    {:status 301
     :headers {"Location" (str "/" id "/edit" "?" (:query-string req))
               "Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}}

    (get-in hs "x-body")
    {:status 200
     :body (render/render-doc ztx {:request req :doc doc} doc)}

    :else
    {:status 200
     :body (render/doc-view ztx {:request req :doc doc} doc)}))

(defmethod web/middleware-out 'zd/layout
  [ztx _cfg {page :page {lay-sym :layout} :zd/config :as req} {bdy :body :as resp} & args]
  (when (and (not (string? bdy)) (= 200 (:status resp)))
    {:headers {"Content-Type" "text/html"}
     :body (methods/layout ztx (zen/get-symbol ztx lay-sym) bdy page)}))

(defmethod zen/op 'zd/render-widget
  [ztx _cfg {{id :id wgt :widget-id} :route-params :keys [doc] :as req} & opts]
  (if-not (nil? doc)
    {:status 200
     :body (methods/widget ztx {:widget (keyword wgt) :request req} doc)}
    {:status 200
     :body [:div "Error: " id " is not found"]}))

(defmethod zen/op 'zd/save-doc
  [ztx _cfg {config :zd/config
             pths :zd/paths
             {id :id} :route-params :as req} & opts]
  ;; TODO emit zen event
  (println :save id)
  (let [lines (slurp (:body req))
        content (->> lines
                     (StringReader.)
                     (io/reader)
                     (line-seq)
                     (map str/trim)
                     (remove #(str/starts-with? % ":zd/docname"))
                     (str/join "\n"))

        doc (->> (reader/parse ztx {:req req} lines)
                 (meta/append-meta ztx)
                 (meta/validate-doc ztx))

        docname (str (:zd/docname doc))]
    (if-let [errs (seq (get-in doc [:zd/meta :errors]))]
      {:status 422 :body {:message "document validation failed"
                          :docname docname
                          :errors errs}}
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
        (spit filename content)
        ;; TODO emit zen event saved succesfully
        ;; TODO load single document into db
        (loader/reload-sync! ztx)
        (zen/op-call ztx 'zd/gitsync-doc {:docpath filename :docname docname})
        {:status 200 :body (str "/" docname)}))))

(defmethod zen/op 'zd/delete-doc
  [ztx _cfg {config :zd/config
             pths :zd/paths
             {:keys [id]} :route-params :as req} & opts]
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
    (zen/op-call ztx 'zd/gitsync-delete-doc {:docpath filepath :docname id})
    ;; TODO load single document into db
    (loader/reload-sync! ztx)
    {:status 200 :body redirect}))

(defmethod zen/op 'zd/render-editor
  [ztx _cfg {{id :id} :route-params :as req} & opts]
  (let [doc (or (:doc req) {:zd/meta {:docname (symbol id)}})]
    {:status 200
     :body (render/editor ztx {:request req :doc doc} doc)}))

(defmethod zen/op 'zd/render-preview
  [ztx _ {{id :id} :route-params :as req} & opts]
  {:headers {"Content-Type" "text/html"}
   :body (-> (render/preview ztx {:request req} (slurp (:body req)))
             (hiccup/html))
   :status 200})

(defmethod zen/start 'zd/db
  [ztx config & opts]
  ;; TODO emit zen event
  (println 'starting-zen-db)
  ;; TODO process error and shutdown gracefully
  (loader/reload-sync! ztx)
  config)

(defmethod zen/stop 'zd/db
  [ztx config & opts]
  ;; TODO emit zen event
  (println 'stopping-zen-db)
  ;; TODO dissoc zendoc state from memory
  )
