;; :related #{comp.auth}
;; :summary "......"
(ns zd.core
  (:require
   [zen.core :as zen]
   [zd.db]
   [zd.pages]
   [zd.web]
   [clojure.walk]
   [edamame.core]
   [cheshire.core]
   [route-map.core :as route-map]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import [java.io InputStream]))


(defn reload [ztx _opts]
  (swap! ztx dissoc :zdb)
  (let [dirs (:zd/paths @ztx)]
    (println "load dirs: " dirs)
    (zd.db/load-dirs ztx dirs))
  :ok)


(defmulti op (fn [ztx {{op :op} :match} req] op))

(defmethod op :default
  [_ {{op :op} :match} _]
  {:status 404 :body (format "can't find %s operation" op)})


(defmulti rpc (fn [_ztx req] (when-let [m (:method req)] (keyword m))))

(defmethod rpc :default
  [_ztx req]
  {:error {:message (str "No method " (:method req))}})

(defn parse-body [b]
  (cond
    (string? b) (cheshire.core/parse-string b keyword)
    (instance? InputStream b) (cheshire.core/parse-stream (io/reader b) keyword)
    :else b))

(defn redirect [url]
  {:status 301
   :headers {"Location" url
             "Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}})

(defmethod op :symbol
  [ztx {{id :id} :params} req]
  (if-let [page (zd.db/get-page ztx (symbol (or id "index")))]
    {:status 200
     :body (zd.pages/render-page ztx (assoc page :request req))}
    (redirect (str "/" id "/edit"))))

(defmethod op :edit
  [ztx {{id :id} :params} req]
  (if-let [page (or (zd.db/get-page ztx (symbol (or id "index"))) {:zd/name (symbol id)})]
    {:status 200
     :body (zd.pages/render-edit-page ztx (assoc page :request req))}
    {:status 404 :body "Ups"}))

(defmethod op :update
  [ztx {{id :id} :params} req]
  (let [content (slurp (:body req))
        name "editable-res"]
    (zd.db/load-content! ztx {:path "" :resource-path name :content content})
    (let [doc (zd.db/get-page ztx (symbol name))]
      {:status 200
       :body (hiccup.core/html (zd.pages/page-content ztx doc))})))

(defmethod op :save
  [ztx {{id :id} :params} {uri :uri :as req}]
  (println :save id)
  (let [content (slurp (:body req))
        file (str "docs/" (str/join "/" (str/split id #"\.")) ".zd")]
    (println :file file)
    (spit file content))
  {:body (str "/" id) :status 200})

(def routes
  {:GET {:op :symbol}
   [:id] {:GET {:op :symbol}
          "rpc"  {:POST {:op :rpc}}
          "edit" {:GET  {:op :edit}
                  :POST {:op :update}
                  :PUT  {:op :save}}}})


(defn dispatch [ztx {uri :uri m :request-method :as req}]
  (println m uri (:params req))
  (when-not (get-in @ztx [:zd/opts :production])
    (reload ztx {}))
  (if-let [match (some->> (get-in @ztx [:zd/opts :route-map])
                          (route-map.core/match [m uri]))]
    (op ztx match (update req :params merge (:params match)))
    {:status 404 :body (str m " " uri " not found")}))


(defn start [ztx opts]
  (swap! ztx assoc :zd/opts (update opts :route-map (fn [x] (merge-with merge (or x {}) routes))))
  (reload ztx opts)
  (zen.core/read-ns ztx 'zd)
  (zd.web/start ztx opts #'dispatch))


(defn stop [ztx]
  (zd.web/stop ztx))

(comment

  (System/getProperty "user.dir")

  (def pth "docs")

  (def pth "/Users/ghrp/prog/zd/docs")

  (def pth "/Users/ganshinv/hs/sansara/box/docs")

  (def pth "/Users/niquola/sansara/box/docs")

  (def pth "/Users/niquola/hs-kb/libs/zd/docs")

  (def pth "/Users/macabre/IdeaProjects/hs/zd/docs")

  (def pth "/home/svt/projects/sansara/zendocs/libs/zd/docs")

  (def ztx (zen/new-context {:zd/paths [pth] :paths [pth]}))

  (start ztx {:port 3031})

  (reload ztx {:port 3031})


  (stop ztx)

  )
