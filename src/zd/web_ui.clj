(ns zd.web-ui
  (:require [zen.core :as zen])
  (:import (javafx.application Application Platform)
           (javafx.scene Scene)
           (javafx.scene.web WebView)
           (javafx.stage Stage)))

(defn start-app [app config]
  (let [web-view (WebView.)
        web-engine (.getEngine web-view)
        scene (Scene. web-view)]
    (.load web-engine (:uri config))
    (.setScene app scene)))

(defmethod zen/start 'zd/web-ui
  [ztx config & opts]
  (println :starting-web-ui)
  #_{:app (doto (proxy [Application] []
                (start [app]
                  (Platform/runLater (fn [] (start-app app config)))))
          (.launch (into-array String [])))}
  'ok)

(def app (proxy [Application] []
           (start [app]
             (Platform/runLater (fn [] #_(start-app app {:uri "http://localhost:4444"}))))))

(comment
  (.stop app)

  (Platform/exit)

  (Application/launch (class app) (into-array String [])))



(defmethod zen/stop 'zd/web-ui
  [ztx config {a :app :as state}]
  (println :stopping-web-ui)
  #_(.stop a (into-array String [])))
