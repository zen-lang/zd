(ns zd.core
  (:require
   [zen.core :as zen]
   [zd.parser]
   [zd.pages]
   [clojure.java.io :as io]
   [clojure.pprint]
   [clojure.string :as str]
   [clojure.walk]
   [edamame.core]
   [zenbox.rpc]
   [zenbox.web.core :as web]))


(defn reload [ztx _opts]
  (swap! ztx dissoc :zd/resources :zd/links)
  (let [pth (:zd/path @ztx)]
    (println "Load " pth)
    (doseq [f (->> (file-seq (io/file pth))
                   (sort-by (fn [x] (.getPath x))))]
      (let [p (.getPath f)]
        (when (and (str/ends-with? p ".zd")
                   (not (str/starts-with? (.getName f) ".")))
          (zd.parser/load-doc-file ztx f)))))
  :ok)

(defmethod zenbox.web.core/operation 'zd/render-symbol
  [ztx op {{sym :symbol} :route-params :as req}]
  (reload ztx {})
  (let [sym (if sym (symbol sym) '_welcome)]
    (if-let [doc (zd.parser/get-doc ztx sym)]
      {:status 200
       :body  (zd.pages/render-page ztx doc)}
      {:status 404
       :body  (str "No page for " sym)})))

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

  (def ztx (zen/new-context {:zd/path pth}))

  (zd.core/start ztx {})

  (zd.parser/get-doc ztx 'aidbox)


  (:zd/resources @ztx)

  (:errors @ztx)

  (start ztx)

  (stop ztx)


  )
