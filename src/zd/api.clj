(ns zd.api
  (:require [zd.loader :as loader]
            [zen.core :as zen]
            [zd.methods :as methods]
            [zd.render :as render]
            [zd.layout]
            [zen-web.core :as web]))

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

(defmethod zen/start 'zd.v2/db
  [ztx config & opts]
  (loader/hard-reload! ztx (:paths config))
  config)

(defmethod zen/stop 'zd.v2/db
  [ztx config & opts]
  ;; TODO dissoc zendoc state from memory
  )

