(ns zd.db
  (:require
   [zen.core :as zen]
   [zd.parse]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn create-resource [ztx res]
  (swap! ztx assoc-in [:zdb (:zd/name res)] res))

(defn read-resource [ztx nm]
  (get-in @ ztx [:zdb nm]))

(defn load-dirs [ztx dirs]
  (doseq [dir dirs]
    (let [dir (io/file dir)
          dir-path (.getPath dir)]
      (doseq [f (->> (file-seq dir)
                     (sort-by (fn [x] (.getPath x))))]
        (let [path (.getPath f)]
          (when (and (str/ends-with? path ".zd")
                     (not (str/starts-with? (.getName f) ".")))
            (let [resource-path (subs path (inc (count dir-path)))
                  resource-name (symbol (str/replace (str/replace resource-path #"\.zd$" "") #"/" "."))
                  content (slurp f)
                  data (zd.parse/parse ztx content)]
              (create-resource
               ztx (assoc data
                          :zd/name resource-name
                          :zd/file resource-path)))))))))
