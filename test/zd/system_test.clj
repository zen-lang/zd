(ns zd.system-test
  (:require
   [zd.api :as api]
   [matcho.core :as matcho]
   [clojure.test :refer [deftest is testing]]
   [zen.core :as zen]))

(defonce ztx (zen/new-context {}))

(comment
  (:zen/state @ztx)
  (zen/errors ztx)

  (def ztx (zen/new-context {})))

(deftest system-config
  (zen/read-ns ztx 'zd)

  (is (= :zen/loaded (zen/read-ns ztx 'zd.test)))
  ;; TODO think about resetting prev errors on reading ns?
  (is (empty? (zen/errors ztx))))

(deftest test-system

  (zen/stop-system ztx)

  (zen/read-ns ztx 'zd)

  (is (= :zen/loaded (zen/read-ns ztx 'zd.test)))

  (zen/start-system ztx 'zd.test/system)

  (is :system-started)

  (zen/stop-system ztx)

  (is :system-stopped))
