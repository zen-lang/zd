(ns zd.core
  (:require
   [zen.core :as zen]
   [zd.db]
   [zd.parser]
   [zd.pages]
   [clojure.pprint]
   [clojure.walk]
   [edamame.core]
   [zenbox.rpc]
   [zenbox.web.core :as web]))


(defn reload [ztx _opts]
  (swap! ztx dissoc :zdb)
  (let [dirs (:zd/paths @ztx)]
    (zd.db/load-dirs ztx dirs))
  :ok)

(defmethod zenbox.web.core/operation 'zd/render-symbol
  [ztx op {{sym :symbol} :route-params :as req}]
  (reload ztx {})
  (let [sym (if sym (symbol sym) 'readme)]
    (if-let [doc (zd.db/read-resource ztx sym)]
      {:status 200
       :body  (zd.pages/render-page ztx doc)}
      {:status 404
       :body  (zd.pages/render-not-found ztx sym)})))

(defmethod zenbox.web.core/operation 'zd/render-zen
  [ztx op {{sym :sym ns :ns} :route-params :as req}]
  (if-let [doc (zen.core/get-symbol ztx (symbol ns sym))]
    {:status 200
     :body  (zd.pages/render-zen ztx doc)}
    {:status 404
     :body  (str "No page for " sym)}))

(defn start [ztx opts]
  (reload ztx opts)
  (zen.core/read-ns ztx 'zd)
  (web/start ztx))

(defn stop [ztx]
  (web/stop ztx))

(comment

  (System/getProperty "user.dir")

  (def pth "docs")

  (def pth "/Users/ghrp/prog/sansara/box/docs")

  (def pth "/Users/ganshinv/hs/sansara/box/docs")

  (def ztx (zen/new-context {:zd/paths [pth]}))

  (start ztx {})

  (stop ztx)

  )
