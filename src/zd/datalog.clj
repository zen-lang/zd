(ns zd.datalog
  (:require
   [zen.core :as zen]
   [xtdb.api :as xt]
   [clojure.string :as str]
   [clojure.walk]
   [clojure.java.io :as io]))


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

(defn stringify [m]
  (clojure.walk/postwalk (fn [x] (if (symbol? x) (str x) x)) m))

(defmethod zen/op 'zd/datalog-sync
  [ztx _config {_ev :ev res :params} & [_session]]
  (let [id (:zd/name res)]
    (submit ztx
            (assoc (stringify (dissoc res :zd/back-links))
                   :xt/id (str id)
                   :parent (str/join "." (butlast (str/split (str id) #"\.")))))))




(comment

  (->> (:zdb @ztx)
       (mapv (fn [[id {res :resource}]]
               (println id (pr-str (assoc res :tx/id (str id))))
               (xt/submit-tx xtdb-node [[::xt/put ]])))
       (count))

  (q '{:find [e]
       :where [[e :parent "people"]]
       :limit 10})

  (q '{:find [e c]
       :where [[e :linked "team.niquola"]
               [e :role "roles.cto"]
               [e :company c]
               [c :tags "tags.ehr"]]
       :limit 10})

  (q '{:find [(pull e [*])]
       :where [[e :xt/id "people.alexander-kiel"]]
       :limit 10})

  )
