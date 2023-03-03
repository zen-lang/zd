(ns zd.macros
  (:require [clojure.string :as str]
            [zd.methods :as methods]
            [clj-yaml.core :as yaml]
            [cheshire.core :as json]
            [sci.core :as sci]))

(defn load-macro [docpath file-path & [fmt]]
  (let [dir (->> (str/split docpath #"/")
                 butlast
                 (str/join "/"))
        content (slurp (str dir "/" file-path))]
    (cond (nil? content) {:error {:message (str "File " file-path " not found")
                                  :dir dir
                                  :docpath docpath}}
          (= :yaml fmt) (yaml/parse-string content)
          (= :json fmt) (json/parse-string content)
          :else content)))

(defmethod methods/eval-macro! 'load
  [ztx doc docpath [op & args :as macro]]
  (let [sci-ctx (sci/init {:bindings {'load load-macro}})
        expr (concat (list op (get-in doc [:zd/meta :path])) args)]
    (try (sci/eval-form sci-ctx expr)
         (catch Exception e
           {:error
            {:message (.getMessage e)
             :docpath docpath
             :type "macro-eval"}}))))
