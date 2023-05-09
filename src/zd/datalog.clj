(ns zd.datalog
  (:require [zen.core :as zen]
            [xtdb.api :as xt]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(defn get-node [ztx]
  (get-in @ztx [:zen/state :datalog :state]))

(defn submit [ztx data]
  (if-let [node (get-node ztx)]
    (xt/submit-tx node [[::xt/put data]])
    :no/xtdb))

(defn query [ztx query & params]
  (if-let [node (get-node ztx)]
    (apply xt/q (xt/db node) query params)
    :no/xtdb))

(defn flatten-doc [ztx {{dn :docname :as m} :zd/meta :as doc}]
  (let [parent-id (->> (str/split (str dn) #"\.")
                       (butlast)
                       (str/join "."))
        meta (->> m
                  (map (fn [[k v]] [(keyword "meta" (name k)) v]))
                  (into {}))
        xtdb-doc
        (-> (dissoc doc :zd/backlinks :zd/subdocs :zd/meta)
            (merge meta)
            (assoc :xt/id (str (:docname m)))
            (assoc :parent parent-id))]
    (walk/postwalk (fn [x] (if (symbol? x) (str x) x))
                   xtdb-doc)))

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
  [ztx config & opts]
  ;; TODO add zen pub sub event
  (println :zd.datalog/start)
  (xt/start-node {:xtdb.lucene/lucene-store {}}))

(defmethod zen/stop 'zd.engines/datalog
  [ztx config state]
  (println :zd.datalog/stop)
  (.close state))
