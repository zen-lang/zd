(ns zd.api-test
  (:require
   [clojure.string :as str]
   [zd.api]
   [matcho.core :as matcho]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [zen.core :as zen]
   [zen-web.core :as web]))

(defonce ztx (zen/new-context {}))

(defn req-body [s]
  (io/input-stream (.getBytes s)))

;; TODO use path prefix from test.edn
(defn read-doc [s]
  (let [f (io/file (str "customers-x/" s))]
    (when (.exists f)
      (slurp f))))

(deftest doc-creation-test

  (zen/stop-system ztx)

  (zen/read-ns ztx 'zd)

  (zen/read-ns ztx 'zd.test)

  (zen/start-system ztx 'zd.test/system)

  (testing "when document not found redirects to editor"
    (matcho/assert
     {:status 301 :headers {"Location" "/testdoc/edit?"}}
     (web/handle ztx 'zd/api {:uri "/testdoc"})))

  (testing "editor config is rendered"
    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api {:uri "/testdoc/edit"})))

  (testing "saving document"
    (matcho/assert
     {:status 422 :body {:message string?}}
     (web/handle ztx 'zd/api
                 {:uri "/testdoc/edit"
                  :request-method :put
                  :body (req-body ":zd/docname testdoc._draft\n:desc /\n no docname present")}))

    (matcho/assert
     {:status 422 :body {:message string?}}
     (web/handle ztx 'zd/api
                 {:uri "/testdoc/edit"
                  :request-method :put
                  :body (req-body ":desc /\n no docname present")}))

    (matcho/assert
     {:status 200 :body string?}
     (web/handle ztx 'zd/api
                 {:uri "/testdoc/edit"
                  :request-method :put
                  :body (req-body ":zd/docname testdoc\n:title \"testdoc\"\n:tags #{}\n:desc /")}))

    (is (not (str/blank? (read-doc "testdoc.zd")))))

  (testing "delete document"
    (matcho/assert
     {:status 200 :body string?}
     (web/handle ztx 'zd/api {:uri "/testdoc" :request-method :delete}))

    (is (nil? (read-doc "testdoc.zd"))))

  (zen/stop-system ztx))

(deftest doc-validation-test
  (zen/stop-system ztx)

  (zen/read-ns ztx 'zd)

  (zen/read-ns ztx 'zd.test)

  (zen/start-system ztx 'zd.test/system)

  (def invalid-doc ":zd/docname customers._draft\n:title #{mytitle}\n:icon \"a string\"")

  (def doc ":zd/docname customers.zero\n:title \"zero inc\"\n:rel #{rel.partner}\n:techs #{techs.clojure}\n:desc \"mydesc\" ")

  (def errs
    [{:type :docname-validation,
      :path [:zd/docname],
      :message "Rename :zd/docname from _draft"}
     ;; two keys defined in _schema
     {:type :doc-validation,
      :message "Expected type of 'string, got 'persistenthashset",
      :path [:title]}
     {:type :doc-validation,
      :message "Expected type of 'vector, got 'string",
      :path [:icon]}
     ;; required in customers._schema
     {:type :doc-validation, :message ":rel is required", :path [:rel]}
     ;; required in _schema
     {:type :doc-validation,
      :path [:desc]}])

  (testing ":zd/docname, keys from both _schema and customers._schema are validated"
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

  (testing "extra key :techs is allowed, required keys pass validation"
    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api
                 {:uri "/customers._draft/edit"
                  :request-method :put
                  :body (req-body doc)}))

    (is (read-doc "customers/zero.zd"))

    (is (= 200 (:status (web/handle ztx 'zd/api {:uri "/customers.zero"
                                                 :request-method :delete})))))

  (testing "keys in subdocuments are validated with _schemas"

    (def doc ":zd/docname customers.uno\n&partners\n:rel tags.client")

    (matcho/assert
     {:status 422
      :body {:errors [{:type :doc-validation
                       :message "Expected type of 'set, got 'symbol"
                       :path [:zd/subdocs :partners :rel]}]}}
     (web/handle ztx 'zd/api
                 {:uri "/customers._draft/edit"
                  :request-method :put
                  :body (req-body doc)}))

    (def doc ":zd/docname customers.uno\n:rel #{rel.client}\n:desc \"best client!\"\n:title \"uno inc.\"\n&partners \n:rel #{rel.unknown}")

    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api
                 {:uri "/customers._draft/edit"
                  :request-method :put
                  :body (req-body doc)}))

    (is (read-doc "customers/uno.zd"))

    (testing ":schema defined for a subdocument &mydoc in _schema.zd"

      (def doc ":zd/docname customers.uno\n:title \"uno inc\"\n:rel #{}\n:tags #{}\n&mydoc\n:rel #{tags.client}")

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
                    :body (req-body doc)})))

    (def doc ":zd/docname customers.uno\n:title \"uno inc\"\n:desc \"uno incorporated\"\n:rel #{}\n&mydoc\n:rel #{tags.client}\n:title \"mytitle\"")

    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api
                 {:uri "/customers.uno/edit"
                  :request-method :put
                  :body (req-body doc)})))

  (testing "cleanup and check"
    (matcho/assert
     {:status 200 :body "/customers"}
     (web/handle ztx 'zd/api {:uri "/customers.uno"
                              :request-method :delete}))

    (is (nil? (read-doc "customers/uno.zd"))))

  (zen/stop-system ztx))

(deftest schema-edit
  (zen/stop-system ztx)

  (zen/read-ns ztx 'zd)

  (zen/read-ns ztx 'zd.test)

  (zen/start-system ztx 'zd.test/system)

  (def doc ":zd/docname partners.boom\n:title \"boom industries\"")

  (testing "root _schema requires :desc"
    (matcho/assert
     {:status 422
      :body {:errors [{:path [:desc]}]}}
     (web/handle ztx 'zd/api
                 {:uri "/partners._draft/edit"
                  :request-method :put
                  :body (req-body doc)})))

  (def sch ":zd/docname partners._schema\n:title \"Schema\"\n:desc /\nschema\n:tags #{}\n:schema {:require #{:category}}")

  (testing "add _schema for partners"
    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api
                 {:uri "/partners._draft/edit"
                  :request-method :put
                  :body (req-body sch)}))
    (is (string? (read-doc "partners/_schema.zd"))))

  (testing ":category is now required in partners._schema"
    (def doc ":zd/docname partners.boom\n:title \"boom industries\"\n:tags #{}")

    (matcho/assert
     {:status 422}
     (web/handle ztx 'zd/api
                 {:uri "/partners._draft/edit"
                  :request-method :put
                  :body (req-body doc)}))

    (def doc ":zd/docname partners.boom\n:title \"boom industries\"\n:desc \"mydesc\"\n:category \"E-commerce\"")

    (matcho/assert
     {:status 200}
     (web/handle ztx 'zd/api
                 {:uri "/partners._draft/edit"
                  :request-method :put
                  :body (req-body doc)}))
    (is (string? (read-doc "partners/boom.zd"))))

  (testing "delete created docs"
    (matcho/assert
     {:status 200 :body string?}
     (web/handle ztx 'zd/api {:uri "/partners._schema"
                              :request-method :delete}))

    (is (nil? (read-doc "partners/_schema.zd")))

    (matcho/assert
     {:status 200 :body string?}
     (web/handle ztx 'zd/api {:uri "/partners.boom"
                              :request-method :delete}))
    (is (nil? (read-doc "partners/boom.zd"))))

  (zen/stop-system ztx))

(defn restart! [ztx]
  (zen/stop-system ztx)
  (zen/start-system ztx 'zd.test/system))

(comment
  (restart! ztx))
