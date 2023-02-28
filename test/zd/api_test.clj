(ns zd.api-test
  (:require
   [matcho.core :as matcho]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [zen.core :as zen]
   [zd.core]
   [zen-web.core :as web]))

(defonce ztx (zen/new-context {}))

(defn prepare! []
  (zen/read-ns ztx 'zd.test-system))

(defn req-body [s]
  (io/input-stream (.getBytes s)))

(deftest doc-creation-test

  (prepare!)

  (zen/start-system ztx 'zd.test-system/system)

  (matcho/assert
   {:status 301 :headers {"Location" "/index/edit?"}}
   (web/handle ztx 'zd.test-system/api {:uri "/index"}))

  (matcho/assert
   {:status 200 :body string?}
   (web/handle ztx 'zd.test-system/api {:uri "/index/edit"
                                     :request-method :put
                                     :body (req-body ":zd/docname index\n:desc /")}))

  (is (io/resource "zd/tdocs/index.zd"))

  (is (= (slurp (io/resource "zd/tdocs/index.zd")) ":desc /"))

  #_(testing "rename with zd/docname"
    (web/handle ztx 'zd.hsm-test/api {:uri "/index/edit"
                                      :request-method :put
                                      :body (req-body ":zd/docname readme.index\n:title \"mytitle\"\n:desc /")})

    (is (nil? (io/resource "zd/tdocs/index.zd")))
    (is (io/resource "zd/tdocs/readme/index.zd"))

    (is (= (slurp (io/resource "zd/tdocs/readme/index.zd"))
           ":title \"mytitle\"\n:desc /")))

  ;; TODO upd backrefs

  (matcho/assert
   {:status 200 :body "/index"}
   (web/handle ztx 'zd.test-system/api {:uri "/index" :request-method :delete}))

  (is (nil? (io/resource "zd/tdocs/index.zd")))

  (zen/stop-system ztx))
