(ns zd.api
  (:require
   [clojure.walk :as walk]
   [xtdb.api :as xt]
   [hiccup.core :as hiccup]
   [zd.loader :as loader]
   [zd.parser :as parser]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [zen.core :as zen]
   [zd.methods :as methods]
   [zd.render :as render]
   [zd.layout]
   [zen-web.core :as web])
  (:import [java.io StringReader]))

(defmethod web/middleware-in 'zd.v2/append-config
  [ztx {cfg-symbol :zendoc-config} req & opts]
  (let [{pths :paths :as zendoc-cfg} (zen/get-symbol ztx cfg-symbol)]
    {:zd/config zendoc-cfg
     :zd/paths pths}))

(defmethod web/middleware-in 'zd.v2/append-doc
  [ztx _cfg {{id :id} :route-params :as req} & opts]
  {:doc (loader/get-doc ztx (symbol id))})

(defmethod zen/op 'zd.v2/render-doc
  [ztx cfg {{id :id} :route-params
            config :zd/config
            doc :doc :as req} & opts]
  (cond
    (nil? doc)
    {:status 301
     :headers {"Location" (str "/" id "/edit" "?" (:query-string req))
               "Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}}

    :else
    {:status 200
     :body (render/doc-view ztx {:request req :doc doc} config doc)}))

(defmethod web/middleware-in 'zd.v2/dev-reload
  [ztx cfg {config :zd/config} & args]
  (loader/reload ztx config))

(defmethod web/middleware-out 'zd.v2/layout
  [ztx _cfg {page :page {lay-sym :layout} :zd/config :as req} {bdy :body :as resp} & args]
  (when (and (not (string? bdy)) (= 200 (:status resp)))
    {:headers {"Content-Type" "text/html"}
     :body (methods/layout ztx (zen/get-symbol ztx lay-sym) bdy page)}))

(defmethod zen/op 'zd.v2/render-widget
  [ztx _cfg {{id :id wgt :widget-id} :route-params :keys [doc] :as req} & opts]
  (if-not (nil? doc)
    {:status 200
     :body (methods/widget ztx wgt doc)}
    {:status 200
     :body [:div "Error: " id " is not found"]}))

(defmethod zen/op 'zd.v2/save-doc
  [ztx _cfg {pths :zd/paths {id :id} :route-params :as req} & opts]
  (println :save id)
  (let [lines (->> (slurp (:body req))
                   (StringReader.)
                   (io/reader)
                   (line-seq)
                   (map str/trim))

        docname
        (->> lines
             (drop-while #(not (str/starts-with? % ":zd/docname")))
             first
             (parser/split #(= % \space))
             second
             (rest)
             (apply str))

        content (->> lines
                     (remove #(str/starts-with? % ":zd/docname"))
                     (str/join "\n"))]

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
            (spit filename content)
            ;; TODO load single document into db
            (loader/hard-reload! ztx pths)
            {:status 200 :body (str "/" docname)}))))

(defmethod zen/op 'zd.v2/delete-doc
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
    (loader/hard-reload! ztx pths)
    {:status 200 :body redirect}))

(defmethod zen/op 'zd.v2/render-editor
  [ztx _cfg {{id :id} :route-params :as req} & opts]
  (let [doc (or (:doc req) {:zd/meta {:docname (symbol id)}})]
    {:status 200
     :body (render/editor ztx {:request req :doc doc} doc)}))

(defmethod zen/op 'zd.v2/render-preview
  [ztx _ {{id :id} :route-params :as req} & opts]
  {:headers {"Content-Type" "text/html"}
   :body (-> (render/preview ztx {:request req} (slurp (:body req)))
             (hiccup/html))
   :status 200})

(defn get-node [ztx]
  (get-in @ztx [:zen/state :datalog :state]))

(defn submit [ztx data]
  (if-let [node (get-node ztx)]
    (xt/submit-tx node [[::xt/put data]])
    :no/xtdb))

(defn query [ztx query]
  (if-let [node (get-node ztx)]
    (xt/q (xt/db node) query)
    :no/xtdb))

(defn stringify [m]
  (walk/postwalk (fn [x] (if (symbol? x) (str x) x)) m))

(defmethod zen/op 'zd.v2/query
  [ztx config params & [session]]
  (query ztx params))

(defmethod zen/op 'zd.v2/submit
  [ztx _config params & [_session]]
  (submit ztx params))

(defmethod zen/op 'zd.v2/datalog-sync
  [ztx _config {_ev :ev doc :params} & [_session]]
  (println 'on-doc-create)
  (let [id (get-in doc [:zd/meta :docname])]
    (submit ztx
            (assoc (stringify (dissoc doc :zd/back-links :zd/invalid-links))
                   :xt/id (str id)
                   :parent (str/join "." (butlast (str/split (str id) #"\.")))))))

(defmethod zen/start 'zd.v2/db
  [ztx config & opts]
  ;; TODO emit zen event
  (println 'starting-zen-db)
  (loader/hard-reload! ztx (:paths config))
  config)

(defmethod zen/stop 'zd.v2/db
  [ztx config & opts]
  ;; TODO emit zen event
  (println 'stopping-zen-db)
  ;; TODO dissoc zendoc state from memory
  )

(defmethod zen/start 'zd.v2/datalog
  [ztx config & opts]
  (println 'starting-datalog)
  (xt/start-node {}))

(defmethod zen/stop 'zd.v2/datalog
  [ztx config state]
  (println 'stopping-datalog)
  (.close state))

