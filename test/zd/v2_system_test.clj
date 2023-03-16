(ns zd.v2-system-test
  (:require
   [zd.api :as api]
   [matcho.core :as matcho]
   [clojure.test :refer [deftest is testing]]
   [zen.core :as zen]))

(defonce ztx (zen/new-context {}))

(comment
  (def ztx (zen/new-context {})))

(deftest system-config
  (zen/read-ns ztx 'zd.v2)

  (is (= :zen/loaded (zen/read-ns ztx 'zd.v2-test)))
  ;; TODO think about resetting prev errors on reading ns?
  (is (empty? (zen/errors ztx))))

(deftest test-system

  (zen/stop-system ztx)

  (zen/read-ns ztx 'zd.v2)

  (is (= :zen/loaded (zen/read-ns ztx 'zd.v2-test)))

  (zen/get-symbol ztx 'zd.v2/api)

  (zen/start-system ztx 'zd.v2-test/system)

  (is :system-started)

  (zen/stop-system ztx)

  (is :system-stopped))
