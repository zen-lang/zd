(ns zd.utils
  (:require [clojure.pprint :as ppr]))

(defn safecall [f err]
  (fn [& args]
    (try (let [r (apply f args)]
           {:type :zd.utils/safecall :result r})
         (catch Exception e
           (let [err* (assoc err
                             :message (.getMessage e)
                             :trace (.getStackTrace e))]
             ;; TODO emit zen pub sub event
             (println :safecall-error)
             (ppr/pprint err*)
             err*)))))
