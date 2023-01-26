;; :related #{comp.auth}
;; :summary "......"
(ns zd.core
  (:require
   [zd.parse]
   [zen.core :as zen]
   [zd.db]
   [zd.pages :as pages]
   [zd.web]
   [clojure.walk]
   [edamame.core]
   [cheshire.core]
   [route-map.core :as route-map]
   [clojure.java.io :as io]
   [ring.util.response]
   [clojure.string :as str]
   [stylo.core :refer [c]]
   [gcp.storage])
  (:import [java.io InputStream] [java.nio.file Files CopyOption StandardCopyOption FileSystems]))

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
    (redirect (str "/" id "/edit" "?" (:query-string req)))))

(defmethod op :edit
  [ztx {{id :id} :params} req]
  (let [page (or (zd.db/get-page ztx (symbol id))
                 {:zd/name (symbol id)})]
    {:status 200
     :body (zd.pages/render-edit-page ztx (assoc page :request req))}))

(defmethod op :update
  [ztx {{id :id} :params} req]
  (let [content (slurp (:body req))
        name "editable-res"]
    (zd.db/load-content! ztx {:path "" :resource-path name :content content})
    (let [doc (zd.db/get-page ztx (symbol name))]
      {:status 200
       :body (hiccup.core/html (zd.pages/page-content ztx doc))})))

(defmethod op :editor
  [ztx {{id :id} :params} req]
  {:status 200
   :body (hiccup.core/html [:html
                            [:head
                             [:script {:src "/js/core.js"}]
                             [:script {:src "/js/editor.js"}]]
                            [:body]])})

(defmethod op :preview
  [ztx _ {{id :id} :params :as req}]
  {:body (zd.pages/preview ztx (slurp (:body req)) {:name id :zd/file (str (str/replace id #"\." "/") ".zd")})
   :status 200})

(defmethod op :save
  [ztx {{id :id} :params} {uri :uri :as req}]
  (println :save id)
  (let [content (slurp (:body req))
        parts (str/split id #"\.")
        doc (zd.parse/parse ztx content)
        dirname (str/join "/" (butlast parts))]
    ;; TODO load the document into the db
    (if (= (last parts) "_draft")
      (if-let [docname (not-empty (get-in doc [:resource :zd/filename]))]
        (let [filename
              (str (str/join (butlast parts))
                   (-> (str "/" docname)
                       str/lower-case
                       (str/replace #"\s" "")))]
          (.mkdirs (io/file (str "docs/" dirname)))
          (spit (str "docs/" filename ".zd") content)
          {:status 200 :body (str "/" (str/replace filename "/" "."))})
        {:status 422 :body "Add :zd/filename"})
      (let [filename (str/join "/" parts)]
        (.mkdirs (io/file (str "docs/" dirname)))
        (spit (str "docs/" filename ".zd") content)
        {:status 200 :body (str "/" id)}))))

(defmethod op :save-file
  [ztx {{id :id} :params} req]
  (let [file-name (str (get-in req [:headers "file-name"]))
        dir-path (str "files/" id)
        file-path (str dir-path "/" file-name)
        file (io/file file-path)]
    (Files/copy ^java.io.InputStream (:body req)
                ^java.nio.file.Path (.toPath file)
                (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))
    {:body (cheshire.core/generate-string {:file file-path
                                           :name file-name})
     :status 200}))

(defmethod op :file
  [ztx {{id :id file-name :file} :params} req]
  (let [file-path (str "files/" id "/" file-name)
        file (io/file file-path)]
    (if (.exists file)
      (ring.util.response/file-response file-path)
      {:status 404})))

(defmethod op :git-lfs-upload
  [ztx {{id :id file-name :file} :params} req]
  (println :upload req)
  {:status 200
   :body (cheshire.core/generate-string {:message "Not impl"})})

(defmethod op :git-lfs-batch
  [ztx {{id :id file-name :file} :params} req]
  (let [params (cheshire.core/parse-string (slurp (:body req)) keyword)
        ctx {:account  (gcp.storage/get-sa)}
        objs (->> (:objects params)
                  (mapv (fn [{oid :oid size :size}]
                          {:oid oid
                           :size size
                           :actions
                           {:upload   {:href (gcp.storage/generate-signed-url (assoc ctx :object  oid :method  "PUT"))}
                            :download {:href (gcp.storage/generate-signed-url (assoc ctx :object  oid :method  "GET"))}}})))
        resp {:transfer "basic"
              :objects objs}]
    (println :batch  objs)
    {:status 200
     :headers {"content-type" "application/vnd.git-lfs+json"}
     :body (cheshire.core/generate-string resp)}))

(defmulti widget (fn [ztx wgt page & [opts]] (keyword wgt)))

(defmethod widget
  :default
  [ztx wgt page & [opts]]
  [:div {:class (c [:text :red-500])}
   "Widget - " (pr-str wgt) " is not implemented"])

(defmethod op :widget
  [ztx {{id :id wgt :widget} :params} req]
  (if-let [page (zd.db/get-page ztx (symbol (or id "index")))]
    {:status 200
     :body (hiccup.core/html (widget ztx wgt page))}
    {:status 200
     :body (hiccup.core/html [:div "Error: " id " is not found"])}))

(def routes
  {:GET {:op :symbol}
   ;; TODO remove editor?
   "editor" {:GET {:op :editor}}
   ;; TODO review this api
   "git-lfs" {"objects" {"batch" {:POST {:op :git-lfs-batch}}}
              "upload" {:PUT {:op :git-lfs-upload}
                        :GET {:op :git-lfs-upload}}}
   "rpc"  {:POST {:op :rpc}}
   [:id] {:GET {:op :symbol}
          "preview" {:POST {:op :preview}}
          "widgets" {[:widget] {:GET {:op :widget}}}
          :POST {:op :save}
          [:file] {:GET {:op :file}}
          "file" {:POST {:op :save-file}}
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

  (def pth "/Users/niquola/marketing/libs/zd/docs")

  (def ztx (zen/new-context {:zd/paths [pth] :paths [pth]}))

  (start ztx {:port 3031})

  (reload ztx {:port 3031})

  (stop ztx)

  (println
   (gcp.storage/generate-signed-url
    {:account  (gcp.storage/get-sa)
     :bucket  "hsbizdev"
     :object  "index.json"
     :method  "GET"})))
