(ns zd.schema
  (:require
   [clojure.string :as str]
   [zen.core :as zen]))

(defn find-schema [ztx docname]
  ;; TODO collect all schemas and merge them
  (loop [pths (str/split docname #"\.")]
    (if (empty? pths)
      (get-in @ztx [:zdb '_schema])
      (let [sch-symbol (symbol (str (str/join "." pths) "._schema"))]
        (if-let [sch (get-in @ztx [:zdb sch-symbol])]
          sch
          (recur (butlast pths)))))))

(defn zen-schema [ztx docname]
  (let [doc-schema (find-schema ztx docname)
        head {:type 'zen/map
              :validation-type :open}
        doc-keys (->> (:zd/subdocs doc-schema)
                      (remove (fn [[k v]]
                                (= :subdoc (:type v))))
                      (map (fn [[k v]] [k (:schema v)]))
                      (into {:zd/docname {:type 'zen/symbol}}))
        meta-sch
        {:type 'zen/map
         :keys {:doc {:type 'zen/vector
                      :every {:type 'zen/keyword}}
                :text-values {:type 'zen/any}
                :errors {:type 'zen/any}
                ;; TODO add validation of ann from _schema
                :ann {:type 'zen/map
                      :values {:every
                               {:type 'zen/map
                                :keys {:zd/content-type
                                       {:type 'zen/keyword}}}}}}}
        subdocs
        {:type 'zen/map
         :validation-type :open
         ;; TODO add validation of subdocs
         :values (assoc head :keys (merge doc-keys {:zd/meta meta-sch}))
         :keys (->> (:zd/subdocs doc-schema)
                    (filter (fn [[_ v]]
                              (= :subdoc (:type v))))
                    (map (fn [[k v]]
                           [k (:schema v)]))
                    (into {}))}]
    (-> {:zen/name 'zd.schema/document :tags #{'zen/schema}}
        (merge head)
        (merge (:schema doc-schema))
        (assoc-in [:keys :zd/meta] meta-sch)
        (assoc-in [:keys :zd/subdocs] subdocs)
        (update :keys merge doc-keys))))

(defn validate-doc [ztx doc]
  (let [docname (str (:zd/docname doc))]
    (if-let [sch (zen-schema ztx docname)]
      (let [errs (->> (zen/validate-schema ztx sch doc)
                      (:errors)
                      (map (fn [e] {:type :doc-validation
                                    :message (:message e)
                                    :path (:path e)})))]
        (update-in doc [:zd/meta :errors] into
                   (cond-> errs
                     (or (empty? docname) (str/ends-with? docname "."))
                     (conj {:type :docname-validation
                            :path [:zd/docname]
                            :message "Add not empty :zd/docname"})

                     (str/ends-with? docname "_draft")
                     (conj {:type :docname-validation
                            :path [:zd/docname]
                            :message "Rename :zd/docname from _draft"}))))
      ;; TODO emit zen event
      (do (println ":schema-not-found " docname) doc))))
