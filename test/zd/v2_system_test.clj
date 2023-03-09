(ns zd.v2-system-test
  (:require
   [zd.api :as api]
   [zd.layout]
   [zd.loader :as loader]
   [matcho.core :as matcho]
   [clojure.test :refer [deftest is testing]]
   [zen.core :as zen]
   [zd.core]
   [zen-web.core :as web]))

(defonce ztx (zen/new-context {}))

(comment
  (def ztx (zen/new-context {})))

(defmethod zen/start 'zd.v2-system/db
  [ztx config & opts]
  (loader/hard-reload! ztx (:paths config))
  config)

(defmethod zen/stop 'zd.v2-system/db
  [ztx config & opts]
  ;; TODO dissoc zendoc state from memory
  )

(deftest system-config
  (is (= :zen/loaded (zen/read-ns ztx 'zd.test-system)))
  ;; TODO think about resetting prev errors on reading ns?
  (is (empty? (zen/errors ztx))))

(deftest test-system

  (zen/stop-system ztx)

  (zen/read-ns ztx 'zd.v2-system)

  (zen/start-system ztx 'zd.v2-system/system)

  (is :system-started)

  (zen/stop-system ztx)

  (is :system-stopped))
