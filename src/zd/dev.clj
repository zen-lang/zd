(ns zd.dev
  (:require
   [zen.core :as zen]
   [zd.core]))

(defn -main [& args]
  (println "Starting zen-doc")
  (println "dirs:"  args)
  (let [ztx (zen/new-context {:zd/paths (vec args)
                              :paths (vec args)})]
    (zd.core/start ztx {})))
