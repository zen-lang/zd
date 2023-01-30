(ns zd.datalog
  (:require [xtdb.api :as xt]
            [clojure.string :as str]
            [clojure.java.io :as io]))


(comment

  (defn start-xtdb! []
    (xt/start-node {}))

  (def xtdb-node (start-xtdb!))

  ;; note that attempting to eval this expression more than once before first calling `stop-xtdb!` will throw a RocksDB locking error
  ;; this is because a node that depends on native libraries must be `.close`'d explicitly

  (defn stop-xtdb! []
    (.close xtdb-node))

  (stop-xtdb!)


  (xt/submit-tx xtdb-node [[::xt/put {:xt/id "hi2u" :zd/name 'hi2u :user/name "zig" :tags #{"human"}}]])

  (xt/submit-tx xtdb-node [[::xt/put {:xt/id "people.mark" :user/name "Mark" :linked #{"team.niquola"}}]])


  (defn q [qw]
    (xt/q (xt/db xtdb-node) qw))

  (q '{:find [e]
       :where [[e :user/name _]
               [e :linked "team.niquola"]]})

  (q '{:find [(pull e [*])]
       :where [[e :user/name _]
               [e :tags "human"]]})
  (def ztx @zd.hsm/dtx)

  (:resource (second (first (:zdb @ztx))))

  (defn stringify [m]
    (clojure.walk/postwalk (fn [x] (if (symbol? x) (str x) x)) m))

  (->> (:zdb @ztx)
       (mapv (fn [[id {res :resource}]]
               (println id (pr-str (assoc res :tx/id (str id))))
               (xt/submit-tx xtdb-node [[::xt/put (assoc (stringify (dissoc res :zd/back-links))
                                                         :xt/id (str id)
                                                         :parent (str/join "." (butlast (str/split (str id) #"\."))))]])))
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
