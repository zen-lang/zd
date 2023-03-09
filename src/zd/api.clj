(ns zd.api
  (:require [zd.loader :as loader]
            [zen.core :as zen]
            [zd.render :as render]
            [zen-web.core :as web]))

(defmethod web/middleware-in 'zd.v2-system/append-config
  [ztx {cfg-symbol :zendoc-config} req & opts]
  (let [{pths :paths :as zendoc-cfg} (zen/get-symbol ztx cfg-symbol)]
    {:zd/config zendoc-cfg
     :zd/paths pths}))

(defmethod web/middleware-in 'zd.v2-system/append-doc
  [ztx _cfg {{id :id} :route-params :as req} & opts]
  {:doc (loader/get-doc ztx (symbol id))})

(defmethod zen/op 'zd.v2-system/render-doc
  [ztx cfg {{id :id} :route-params
            config :zendoc/config
            doc :doc :as req} & opts]
  (cond
    (nil? doc)
    {:status 301
     :headers {"Location" (str "/" id "/edit" "?" (:query-string req))
               "Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}}

    :else
    {:status 200
     :body (render/doc-view ztx {:request req :doc doc} config doc)}))
