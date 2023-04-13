(ns zd.datalog-test
  (:require
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

  ;; TODO how to check tx status in xtdb?
  (Thread/sleep 2000)

  (matcho/assert
   #{["customers.flame"]}
   (datalog/query ztx '{:find [e]
                        :where [[e :parent "customers"]]}))

  (matcho/assert
   #{[{:xt/id "people.john" :name "John"}]}
   (datalog/query ztx '{:find [(pull e [:xt/id :name])]
                        :where [[e :role "ceo"]]}))

  (zen/stop-system ztx))

