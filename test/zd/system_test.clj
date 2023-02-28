(ns zd.system-test
  (:require
   [matcho.core :as matcho]
   [clojure.test :refer [deftest is testing]]
   [zen.core :as zen]
   [zd.core]
   [zen-web.core :as web]))

(defonce ztx (zen/new-context {}))

(deftest test-system

  (zen/read-ns ztx 'zd.test-system)

  (zen/start-system ztx 'zd.test-system/system)

  (is :system-started)

  (zen/stop-system ztx)

  (is :system-stopped))
