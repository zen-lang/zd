(ns zd.datalog-test
  (:require [zd.api :as api]
            [clojure.test :refer [deftest is testing]]
            [zen.core :as zen]
            [matcho.core :as matcho]))

(defonce ztx (zen/new-context {}))

(comment
  (def ztx (zen/new-context {})))

(deftest datalog-engine
  (zen/stop-system ztx)

  (zen/read-ns ztx 'zd.v2)

  (zen/read-ns ztx 'zd.v2-test)

  (zen/start-system ztx 'zd.v2-test/system)

  (matcho/assert
   #{[{:xt/id "customers.flame"}]}
   (api/query ztx '{:find [(pull e [:xt/id])]
                    :where [[e :parent "customers"]]}))

  (matcho/assert
   #{[{:xt/id "people.john" :name "John"}]}
   (api/query ztx '{:find [(pull e [:xt/id :name])]
                    :where [[e :role "ceo"]]}))

  (zen/stop-system ztx))

