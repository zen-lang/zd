(ns zd.api-test
  (:require
   [matcho.core :as matcho]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [zen.core :as zen]
   [zd.core]
   [zen-web.core :as web]))

(defonce ztx (zen/new-context {}))

(defn req-body [s]
  (io/input-stream (.getBytes s)))

(deftest doc-creation-test

  (zen/stop-system ztx)

  (zen/read-ns ztx 'zd.v2)

  (zen/read-ns ztx 'zd.v2-test)

  (zen/start-system ztx 'zd.v2-test/system)

  (testing "when document not found redirects to editor"
    (matcho/assert
     {:status 301 :headers {"Location" "/index/edit?"}}
     (web/handle ztx 'zd.v2-test/api {:uri "/index"})))

  (testing "saving document"
    (matcho/assert
     {:status 422 :body string?}
     (web/handle ztx 'zd.v2-test/api
                 {:uri "/index/edit"
                  :request-method :put
                  :body (req-body ":zd/docname index._draft\n:desc /\n no docname present")}))

    (matcho/assert
     {:status 422 :body string?}
     (web/handle ztx 'zd.v2-test/api
                 {:uri "/index/edit"
                  :request-method :put
                  :body (req-body ":desc /\n no docname present")}))

    (matcho/assert
     {:status 200 :body string?}
     (web/handle ztx 'zd.v2-test/api
                 {:uri "/index/edit"
                  :request-method :put
                  :body (req-body ":zd/docname index\n:desc /")}))

    (is (io/resource "zd/tdocs/index.zd"))

    (is (= (slurp (io/resource "zd/tdocs/index.zd")) ":desc /")))

  #_(testing "rename with zd/docname"
      (web/handle ztx 'zd.hsm-test/api {:uri "/index/edit"
                                        :request-method :put
                                        :body (req-body ":zd/docname readme.index\n:title \"mytitle\"\n:desc /")})

      (is (nil? (io/resource "zd/tdocs/index.zd")))
      (is (io/resource "zd/tdocs/readme/index.zd"))

      (is (= (slurp (io/resource "zd/tdocs/readme/index.zd"))
             ":title \"mytitle\"\n:desc /")))

  (testing "delete document"
    (matcho/assert
     {:status 200 :body "/index"}
     (web/handle ztx 'zd.v2-test/api {:uri "/index" :request-method :delete}))

    (is (nil? (io/resource "zd/tdocs/index.zd"))))

  (zen/stop-system ztx))
