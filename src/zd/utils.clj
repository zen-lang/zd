(ns zd.utils
  (:require [clojure.pprint :as ppr]))

(defn safecall [f err]
  (fn [& args]
    (try (apply f args)
         (catch Exception e
           (let [err* (assoc err :message (.getMessage e))]
             ;; TODO emit zen pub sub event
             (println :safecall-error)
             (ppr/pprint err*)
             err*)))))
