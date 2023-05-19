(ns zd.datalog-test
  (:require
   [xtdb.api :as xtdb]
   [zd.api]
   [zd.datalog :as datalog]
   [clojure.test :refer [deftest is testing]]
   [zen.core :as zen]
   [matcho.core :as matcho]))

(defonce ztx (zen/new-context {}))

(comment
  (def ztx (zen/new-context {})))

(deftest datalog-engine
  (zen/stop-system ztx)

  (zen/read-ns ztx 'zd)

  (zen/read-ns ztx 'zd.test)

  (zen/start-system ztx 'zd.test/system)

  (xtdb/sync (:node (datalog/get-state ztx)))

  (testing "metadata is loaded into xtdb"
    (matcho/assert
     #{["customers"]}
     (datalog/query ztx '{:find [?id]
                          :where [[?e :meta/docname docname]
                                  [?e :xt/id ?id]]
                          :in [docname]}
                    'customers)))

  (matcho/assert
   #{["people"] ["_schema"] ["customers.flame"]}
   (datalog/query ztx '{:find [e]
                        :where [[e :parent parent]]
                        :in [parent]}
                  'customers))

  (matcho/assert
   #{[{:xt/id "people.john" :name "John"}]}
   (datalog/query ztx '{:find [(pull e [:xt/id :name])]
                        :where [[e :role "ceo"]]}))

  (zen/stop-system ztx))

