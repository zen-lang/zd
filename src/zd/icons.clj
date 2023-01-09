(ns zd.icons
  (:require [clj-yaml.core]
            [clojure.java.io :as io]))

(def icons
  (->> (clj-yaml.core/parse-string (slurp (io/resource "icons.yml")))
       (mapcat (fn [[k x]]
                 (->> (:styles x)
                      (mapv (fn [x] {:name (format "[:fa-%s :fa-%s]" x (name k))
                                    :icon [(str "fa-" x) (str "fa-" (name k))]})))))))
