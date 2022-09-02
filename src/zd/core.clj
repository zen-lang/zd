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
   [route-map.core :as route-map]
   [clojure.string :as str]))


(defn reload [ztx _opts]
  (swap! ztx dissoc :zdb)
  (let [dirs (:zd/paths @ztx)]
    (println "load dirs: " dirs)
    (zd.db/load-dirs ztx dirs))
  :ok)


(defmulti op (fn [ztx {{op :op} :match} req] op))

(defmethod op :default [_ {{op :op} :match} _] op)

(defn parse-uri [uri]
  (let [edit-postfix "/_edit"
        edit? (str/ends-with? uri "/_edit")
        uri (str/replace uri (re-pattern edit-postfix) "")]
    {:sym (symbol (subs uri 1))
     :edit? edit?
     :uri uri}))

(defn dispatch [ztx {uri :uri m :request-method :as req}]
  (when-not (get-in @ztx [:zd/opts :production])
    (reload ztx {}))
  (if-let [match (when-let [routes (get-in @ztx [:zd/opts :route-map])]
                   (route-map.core/match  [m uri] routes))]
    (op ztx match req)
    (let [{:keys [sym edit? uri]} (parse-uri uri)
          page (zd.db/get-page ztx sym)]
      (cond
        (not page)
        {:status 404
         :body  (zd.pages/render-page ztx {:zd/name sym})}

        edit?
        {:status 200
         :body  (zd.pages/edit-page ztx (assoc page
                                               :request req
                                               :edit? true
                                               :uri uri))}

        :else
        {:status 200
         :body  (zd.pages/render-page ztx (assoc page :request req))}))))


(defn start [ztx opts]
  (swap! ztx assoc :zd/opts opts)
  (reload ztx opts)
  (zen.core/read-ns ztx 'zd)
  (zd.web/start ztx opts dispatch))


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
