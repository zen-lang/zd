(ns zd.api-test
  (:require
   [zd.api]
   [matcho.core :as matcho]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [zen.core :as zen]
   [zen-web.core :as web]))

(defonce ztx (zen/new-context {}))

(defn req-body [s]
  (io/input-stream (.getBytes s)))

(deftest doc-creation-test

  (zen/stop-system ztx)

  (zen/read-ns ztx 'zd)

  (zen/read-ns ztx 'zd.test)

  (zen/start-system ztx 'zd.test/system)

  (testing "when document not found redirects to editor"
    (matcho/assert
     {:status 301 :headers {"Location" "/index/edit?"}}
     (web/handle ztx 'zd/api {:uri "/index"})))

  (testing "editor config is rendered"
    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api {:uri "/index/edit"})))

  (testing "saving document"
    (matcho/assert
     {:status 422 :body {:message string?}}
     (web/handle ztx 'zd/api
                 {:uri "/index/edit"
                  :request-method :put
                  :body (req-body ":zd/docname index._draft\n:desc /\n no docname present")}))

    (matcho/assert
     {:status 422 :body {:message string?}}
     (web/handle ztx 'zd/api
                 {:uri "/index/edit"
                  :request-method :put
                  :body (req-body ":desc /\n no docname present")}))

    (matcho/assert
     {:status 200 :body string?}
     (web/handle ztx 'zd/api
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
     (web/handle ztx 'zd/api {:uri "/index" :request-method :delete}))

    (is (nil? (io/resource "zd/tdocs/index.zd"))))

  (zen/stop-system ztx))

(deftest doc-validation-test
  (zen/stop-system ztx)

  (zen/read-ns ztx 'zd)

  (zen/read-ns ztx 'zd.test)

  (zen/start-system ztx 'zd.test/system)

  (def invalid-doc ":zd/docname customers._draft\n:title #{mytitle}\n:rel \"a string\"")

  (def doc ":zd/docname customers.zero\n:title \"zero inc\"\n:rel #{rel.partner}\n:techs #{techs.clojure}")

  (def errs
    [{:type :docname-validation,
      :path [:zd/docname],
      :message "Rename :zd/docname from _draft"}
     {:type :doc-validation,
      :message "Expected type of 'string, got 'persistenthashset",
      :path [:title]}
     {:type :doc-validation,
      :message "Expected type of 'set, got 'string",
      :path [:rel]}])

  (testing ":zd/docname and props from _schema zd are validated"
    (matcho/assert
     {:status 422
      :body
      {:message "document validation failed"
       :docname "customers._draft"
       :errors errs}}
     (web/handle ztx 'zd/api
                 {:uri "/customers._draft/edit"
                  :request-method :put
                  :body (req-body invalid-doc)})))

  (testing "extra props are allowed"
    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api
                 {:uri "/customers._draft/edit"
                  :request-method :put
                  :body (req-body doc)}))

    (is (string? (slurp (io/resource "zd/tdocs/customers/zero.zd"))))

    (is (= 200 (:status (web/handle ztx 'zd/api {:uri "/customers.zero"
                                                         :request-method :delete})))))

  (testing "subdocuments are validated"

    (def doc ":zd/docname customers.uno\n&mycustomdoc\n:rel tags.client")

    (matcho/assert
     {:status 422
      :body {:errors [{:type :doc-validation
                       :message "Expected type of 'set, got 'symbol"
                       :path [:zd/subdocs :mycustomdoc :rel]}]}}
     (web/handle ztx 'zd/api
                 {:uri "/customers._draft/edit"
                  :request-method :put
                  :body (req-body doc)}))

    (def doc ":zd/docname customers.uno\n&mycustomdoc\n:rel #{tags.client}")

    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api
                 {:uri "/customers._draft/edit"
                  :request-method :put
                  :body (req-body doc)}))

    (is (string? (slurp (io/resource "zd/tdocs/customers/uno.zd"))))

    (def doc ":zd/docname customers.uno\n&mydoc\n:rel #{tags.client}")

    (matcho/assert
     {:status 422,
      :body
      {:message "document validation failed",
       :docname "customers.uno",
       :errors
       [{:type :doc-validation,
         :message ":title is required",
         :path [:zd/subdocs :mydoc :title]}]}}
     (web/handle ztx 'zd/api
                 {:uri "/customers.uno/edit"
                  :request-method :put
                  :body (req-body doc)}))

    (def doc ":zd/docname customers.uno\n&mydoc\n:rel #{tags.client}\n:title \"mytitle\"")

    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api
                 {:uri "/customers.uno/edit"
                  :request-method :put
                  :body (req-body doc)})))

  (zen/stop-system ztx)

  (matcho/assert
   {:status 200 :body "/customers"}
   (web/handle ztx 'zd/api {:uri "/customers.uno"
                                    :request-method :delete}))

  (is (nil? (io/resource "zd/tdocs/customers/uno.zd"))))

(defn restart! [ztx]
  (zen/stop-system ztx)
  (zen/start-system ztx 'zd.test/system))

(comment
  (restart! ztx))
