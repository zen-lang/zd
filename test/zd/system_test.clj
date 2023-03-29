(ns zd.system-test
  (:require
   [zd.api :as api]
   [zd.layout]
   [matcho.core :as matcho]
   [clojure.test :refer [deftest is testing]]
   [zen.core :as zen]
   [zd.core]
   [zen-web.core :as web]))

(defonce ztx (zen/new-context {}))

(comment
  (def ztx (zen/new-context {}))

  )

(deftest system-config
  (is (= :zen/loaded (zen/read-ns ztx 'zd.test-system)))
  ;; TODO think about resetting prev errors on reading ns?
  (is (empty? (zen/errors ztx))))

(deftest test-system

  (zen/stop-system ztx)

  (zen/read-ns ztx 'zd.test-system)

  (zen/start-system ztx 'zd.test-system/system)

  (is :system-started)

  (zen/stop-system ztx)

  (is :system-stopped))