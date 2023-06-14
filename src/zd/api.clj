(ns zd.api
  (:require
   [zd.layout :as layout]
   [zd.memstore :as memstore]
   [zd.meta :as meta]
   [zd.datalog]
   [hiccup.core :as hiccup]
   [zd.reader :as reader]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [zen.core :as zen]
   [zd.fs :as fs]
   [zd.methods :as methods]
   [zd.render :as render]
   [zen-web.core :as web])
  (:import [java.io StringReader]))

;; TODO move to zen-web.http
(defn get-state [ztx]
  (->> [:zen/state :http :state]
       (get-in @ztx)))

(defn zendoc-config [ztx]
  (->> [:config :zendoc]
       (get-in (get-state ztx))
       (zen/get-symbol ztx)))

;; ISSUE mw does not work for / request
(defmethod web/middleware-in 'zd/append-doc
  [ztx _cfg {{id :id} :route-params :as req} & opts]
  (when (some? id)
    ;; TODO make root required in render ops with zen sch
    {:zd/root (:root (zendoc-config ztx))
     :doc (memstore/get-doc ztx (symbol id))}))

(defmethod zen/op 'zd/render-doc
  [ztx config {{id :id} :route-params
               uri :uri
               hs :headers
               doc :doc :as req} & opts]
  (let [r (:root (zendoc-config ztx))]
    (cond
      (= uri "/")
      {:status 301
       :headers {"Location" (str "/" r "?" (:query-string req))
                 "Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}}

      (nil? doc)
      {:status 301
       :headers {"Location" (str "/" id "/edit" "?" (:query-string req))
                 "Cache-Control" "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"}}

      (get hs "x-body")
      {:status 200
       :body (render/render-doc ztx {:request req :doc doc :root r} doc)}

      :else
      {:status 200
       :body (render/doc-view ztx {:request req :doc doc :root r} doc)})))

(defmethod web/middleware-out 'zd/layout
  [ztx config {page :page :as req} {bdy :body :as resp} & args]
  (when (and (not (string? bdy)) (= 200 (:status resp)))
    {:headers {"Content-Type" "text/html"}
     :body (layout/sidebar ztx {:request req} bdy)}))

(defmethod zen/op 'zd/render-widget
  [ztx _cfg {{id :id wgt :widget-id} :route-params r :root :keys [doc] :as req} & opts]
  (if-not (nil? doc)
    {:status 200
     :body (methods/widget ztx {:widget (keyword wgt) :root r :request req} doc)}
    {:status 200
     :body [:div "Error: " id " is not found"]}))

(defmethod zen/op 'zd/save-doc
  [ztx _cfg {{id :id} :route-params r :zd/root :as req} & opts]
  ;; TODO emit zen event
  (println :zd.api/save-doc id)
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
                          :root r
                          :errors errs}}
      (do (zen/pub ztx 'zd.events/on-doc-save {:docname docname :content content :root r})
          {:status 200 :body (str "/" docname)}))))

(defmethod zen/op 'zd/delete-doc
  [ztx _cfg {{:keys [id]} :route-params r :zd/root :as req} & opts]
  (println :delete id)
  (let [parts (str/split id #"\.")
        redirect
        (if-let [parent (seq (butlast parts))]
          (str "/" (str/join "." parent))
          (str "/" r))]
    (zen/pub ztx 'zd.events/on-doc-delete {:docname id :root r})
    {:status 200 :body redirect}))

(defmethod zen/op 'zd/render-editor
  [ztx _cfg {{id :id} :route-params r :zd/root :as req} & opts]
  (let [doc (or (:doc req) {:zd/meta {:docname (symbol id)}})]
    {:status 200
     :body (render/editor ztx {:root r :request req :doc doc} doc)}))

(defmethod zen/op 'zd/render-preview
  [ztx _ {{id :id} :route-params r :zd/root :as req} & opts]
  {:headers {"Content-Type" "text/html"}
   :body (-> (render/preview ztx {:request req :root r} (slurp (:body req)))
             (hiccup/html))
   :status 200})
