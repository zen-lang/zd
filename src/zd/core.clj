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
   [zenbox.pg.core]
   [zenbox.rpc :refer [rpc-call]]
   [zenbox.web.core :as web]))


(defn reload [ztx _opts]
  (swap! ztx dissoc :zd/resources :zd/links)
  (let [pth (:zd/path @ztx)]
    (println "Load " pth)
    (doseq [f (file-seq (io/file pth))]
      (println f)
      (let [p (.getPath f)]
        (when (and (str/ends-with? p ".zd")
                   (not (str/starts-with? (.getName f) ".")))
          (println p)
          (zd.parser/load-file ztx f)))))
  :ok)



(defmulti operation (fn [ztx op req] (:zen/name op)))

(defn rpc [ztx req]
  (if-let [op (zen/get-symbol ztx (:method req))]
    (if-let [schemas (:params op)]
      (let  [{:keys [errors]} (zen/validate ztx schemas (:params req))]
        (if (empty? errors)
          (rpc-call ztx op req)
          {:error errors}))
      (rpc-call ztx op req))
    {:error {:message (str "No operation defined for " (:method req))}}))


(defmethod operation 'zenbox/json-rpc
  [ztx op req]
  (try
    (let [resource (:resource req)
          resp (rpc ztx resource)]
      (if (:result resp)
        {:status 200 :body resp}
        {:status 422 :body resp}))
    (catch Exception e
      (println "ERROR:" e)
      {:status 500 :body {:error (str e)}})))

(defmethod operation 'zd/render-symbol
  [ztx op {{sym :symbol} :route-params :as req}]
  (let [sym (if sym (symbol sym) 'welcome)]
    (if-let [doc (zd.parser/get-doc ztx sym)]
      {:status 200
       :body  (zd.pages/render-page ztx doc)}
      {:status 404
       :body  (str "No page for " sym)})))

(defmethod operation 'zd/render-index
  [ztx op req]
  {:status 200 :body "Index"})


(defmethod rpc-call 'zen-ui/get-symbol
  [ztx rpc {{nm :name} :params}])

(defn dispatch-op [ztx route request]
  (reload ztx {})
  (if route
    (if-let [op (zen/get-symbol ztx (get-in route [:match :operation]))]
      (operation ztx op (assoc request :route-params (:params route)))
      {:status 404 :body "No operation definition"})
    {:status 404 :body "No route"}))

(defn start [ztx opts]
  (reload ztx opts)
  (zen.core/read-ns ztx 'zd)
  (web/start ztx #'dispatch-op))

(defn stop [ztx]
  (web/stop ztx))

(comment

  (System/getProperty "user.dir")

  (def pth "docs")

  (def ztx (zen/new-context {:zd/path pth}))

  (zen.core/read-ns ztx 'zd)

  (zd.core/start ztx {})

  (zd.parser/get-doc ztx 'aidbox)


  (:zd/resources @ztx)

  (:errors @ztx)

  (start ztx)

  (stop ztx)


  )
