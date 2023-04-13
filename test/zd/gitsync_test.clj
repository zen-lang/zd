(ns zd.gitsync-test
  (:require [zd.api]
            [zd.gitsync :as gitsync]
            [zen.core :as zen]
            [clojure.java.io :as io]
            [zen-web.core :as web]
            [clojure.test :refer [deftest is testing]]
            [matcho.core :as matcho])
  (:import org.eclipse.jgit.api.Git))

(defonce ztx (zen/new-context {}))

(defn req-body [s]
  (io/input-stream (.getBytes s)))

;; to run this test setup remote repository
;; and add it to zd/gitsync.remote
(deftest ^:kaocha/pending gitsync

  (zen/stop-system ztx)

  (zen/read-ns ztx 'zd)

  (zen/read-ns ztx 'zd.test)

  (zen/start-system ztx 'zd.test/system)

  (def gitsync-state (get-in @ztx [:zen/state :gitsync :state]))

  ;; timers are ready
  (is (instance? java.util.Timer (:ti gitsync-state)))
  (is (instance? java.util.TimerTask (:task gitsync-state)))

  (is (nil? (agent-errors gitsync/ag)))

  ;; repo is ready
  (is (instance? org.eclipse.jgit.api.Git  (get-in gitsync-state [:remote :repo])))

  (matcho/assert
   {:status 200}
   (web/handle ztx 'zd/api
               {:uri "/_draft/edit"
                :request-method :put
                :body (req-body ":zd/docname index\n:desc /")}))

  (matcho/assert
   {:status 200}
   (web/handle ztx 'zd/api
               {:uri "/index/edit"
                :request-method :put
                :body (req-body ":zd/docname index\n:desc /\na description")}))

  (matcho/assert
   {:status 200 :body "/index"}
   (web/handle ztx 'zd/api {:uri "/index" :request-method :delete}))

  (is (nil? (agent-errors gitsync/ag)))

  (zen/stop-system ztx))
