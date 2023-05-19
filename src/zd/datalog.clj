(ns zd.datalog
  (:require [zen.core :as zen]
            [clojure.java.io :as io]
            [xtdb.api :as xt]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(defn get-state [ztx]
  (get-in @ztx [:zen/state :datalog :state]))

(defn submit [ztx data]
  (if-let [{n :node} (get-state ztx)]
    (xt/submit-tx n [[::xt/put data]])
    :no/xtdb))

(defn query [ztx query & params]
  (if-let [{n :node} (get-state ztx)]
    (apply xt/q (xt/db n) query params)
    :no/xtdb))

(defn flatten-doc [ztx {{dn :docname :as m} :zd/meta :as doc}]
  (let [meta (->> m
                  (map (fn [[k v]] [(keyword "meta" (name k)) v]))
                  (into {}))]
    (-> (dissoc doc :zd/backlinks :zd/subdocs :zd/meta)
        (merge meta)
        (assoc :xt/id (str (:docname m))))))

(defmethod zen/op 'zd/query
  [ztx config params & [session]]
  (query ztx params))

(defmethod zen/op 'zd/submit
  [ztx _config params & [_session]]
  (submit ztx params))

(defmethod zen/op 'zd.events/datalog-sync
  [ztx _config {_ev :ev doc :params} & [_session]]
  (let [xtdb-doc (flatten-doc ztx doc)
        result (submit ztx xtdb-doc)]
    ;; TODO und where does result go in pub/sub
    result))

(defmethod zen/start 'zd.engines/datalog
  [ztx {zd-config :zendoc :as config} & opts]
  ;; TODO add zen pub sub event
  (println :zd.datalog/start)
  ;; TODO use fs directory for lucene index
  (let [{r :root} (zen/get-symbol ztx zd-config)]
    {:config config
     :root r
     :node (xt/start-node {:xtdb.lucene/lucene-store {}})}))

(defmethod zen/stop 'zd.engines/datalog
  [ztx config {n :node}]
  (println :zd.datalog/stop)
  (.close n))
