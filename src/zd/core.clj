(ns zd.core
  (:require [zen.core :as zen]
            [clojure.string :as str]
            [edamame.core]
            [markdown.core]
            [hiccup.core]
            [hiccup.util]
            [clojure.java.io :as io])
  (:import [java.io BufferedReader StringReader]))


(defn start [ztx {pth :kg/path}]
  (doseq [f  (file-seq (io/file pth))]
    (let [p (.getPath f)]
      (when (and (str/ends-with? p ".md")
                 (not (str/starts-with? (.getName f) ".")))
        (let [id (-> (subs p (inc (count pth)))
                     (str/replace #"\.md$" "")
                     (str/replace #"/" "."))]
          (swap! ztx assoc-in [:kg/resources (symbol id)] f)))))
  :ok)

(defn get-resource [ztx nm]
  (get-in @ztx [:kg/resources nm]))
