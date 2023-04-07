(ns zd.datalog
  (:require [zen.core :as zen]
            [xtdb.api :as xt]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(defmethod zen/start 'zd/datalog
  [ztx config & opts]
  ;; TODO add zen pub sub event
  (println 'starting-datalog)
  (xt/start-node {}))

(defmethod zen/stop 'zd/datalog
  [ztx config state]
  (println 'stopping-datalog)
  (.close state))

(defmethod zen/start 'zd/datalog-engine
  [ztx config & opts]
  (xt/start-node {}))

(defmethod zen/stop 'zd/datalog-engine
  [ztx config state]
  (.close state))

(defn get-node [ztx]
  (get-in @ztx [:zen/state :datalog :state]))

(defn submit [ztx data]
  (if-let [node (get-node ztx)]
    (xt/submit-tx node [[::xt/put data]])
    :no/xtdb))

(defn query [ztx query]
  (if-let [node (get-node ztx)]
    (xt/q (xt/db node) query)
    :no/xtdb))

(defmethod zen/op 'zd/query
  [ztx config params & [session]]
  (query ztx params))

(defmethod zen/op 'zd/submit
  [ztx _config params & [_session]]
  (submit ztx params))

(defmethod zen/op 'zd/query
  [ztx config params & [session]]
  (query ztx params))

(defmethod zen/op 'zd/submit
  [ztx _config params & [_session]]
  (submit ztx params))

(defn stringify [m]
  (walk/postwalk (fn [x] (if (symbol? x) (str x) x)) m))

(defmethod zen/op 'zd/datalog-sync
  [ztx _config {_ev :ev doc :params} & [_session]]
  ;; TODO emit zen event
  #_(println 'on-doc-create)
  (let [id (get-in doc [:zd/meta :docname])
        result
        (submit ztx
                ;; TODO think about how to store metadata and zd props in xtdb
                (assoc (stringify (dissoc doc :zd/backlinks :zd/subdocs :zd/invalid-links))
                       :xt/id (str id)
                       :parent (str/join "." (butlast (str/split (str id) #"\.")))))]
    ;; TODO und where does result go in pub/sub
    result))
