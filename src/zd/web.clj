(ns zd.web
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [org.httpkit.server :as http-kit]
   [ring.util.codec :as codec]
   [ring.util.response]
   [ring.util.request]
   [ring.middleware.head]
   [clj-yaml.core]
   [clojure.walk]
   [ring.middleware.content-type])
  (:use [ring.middleware.resource]
        [ring.middleware.file]
        [ring.middleware.not-modified]))

(defn form-decode [s] (clojure.walk/keywordize-keys (ring.util.codec/form-decode s)))

(defn prepare-request [{meth :request-method qs :query-string body :body ct :content-type headers :headers :as req}]
  (let [params (when qs (form-decode qs))
        params (if (string? params) {(keyword params) nil} params)
        method-override (and (= :post meth) (get headers "x-http-method-override"))]
    (cond-> req
      method-override (assoc :request-method (keyword (str/lower-case method-override)))
      params (update :params merge (or params {})))))


(defn preflight
  [{meth :request-method hs :headers :as req}]
  (let [headers (get hs "access-control-request-headers")
        origin (get hs "origin")
        meth  (get hs "access-control-request-method")]
    {:status 200
     :headers {"Access-Control-Allow-Headers" headers
               "Access-Control-Allow-Methods" meth
               "Access-Control-Allow-Origin" origin
               "Access-Control-Allow-Credentials" "true"
               "Access-Control-Expose-Headers" "Location, Transaction-Meta, Content-Location, Category, Content-Type, X-total-count"}}))

(defn allow [resp req]
  (if-let [origin (get-in req [:headers "origin"])]
    (update resp :headers merge
            {"Access-Control-Allow-Origin" origin
             "Access-Control-Allow-Credentials" "true"
             "Access-Control-Expose-Headers" "Location, Content-Location, Category, Content-Type, X-total-count"})
    resp))

(defn handle-static [ztx {meth :request-method uri :uri :as req}]
  (when-let [f (->> (get @ztx :zd/paths)
                    (map (fn [path]
                            (let [f (io/file (str path uri))]
                              (when (.exists f) f))))
                    (first))]
    (ring.util.response/file-response (.getPath f))))

(defn mk-handler [ztx dispatch]
  (fn [req]
    (if (= :options (:request-method req))
      (preflight req)
      (if-let [resp (handle-static ztx req)]
        resp
        (let [req (prepare-request req)
              resp (dispatch ztx req)]
          (-> resp (allow req)))))))

(defn stop [ztx]
  (when-let  [srv (get-in @ztx [:zd/web :server])]
    (println :stop srv)
    (srv)))

(defn start
  [ztx opts dispatch]
  (stop ztx)
  (let [handler (mk-handler ztx dispatch)
        srv (http-kit/run-server handler (merge {:port 3030} opts))]
    (println :start (merge {:port 3030} opts))
    (swap! ztx assoc-in [:zd/web] {:server srv :handler handler}))
  ::started)



(comment
  (def ztx (atom {:zd/paths ["docs"]}))


  (start ztx {:port 3031} (fn [ztx req] {:status 200 :body "Hello"}))

  (stop ztx)
  )


