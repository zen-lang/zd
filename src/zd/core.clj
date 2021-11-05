(ns zd.core
  (:require [zen.core :as zen]
            [zd.parser]
            [clojure.string :as str]
            [clojure.java.io :as io]))


(defn start [ztx _opts]
  (let [pth (:zd/path @ztx)]
    (println "Load " pth)
    (doseq [f (file-seq (io/file pth))]
      (println f)
      (let [p (.getPath f)]
        (when (and (str/ends-with? p ".zd")
                   (not (str/starts-with? (.getName f) ".")))
          (println p)
          (zd.parser/load-file ztx f)))))
  :ok)
