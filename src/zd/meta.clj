(ns zd.meta
  (:require
   [zd.reader :as reader]
   [clojure.string :as str]
   [zen.core :as zen]))

(defn get-group
  "finds keys by :group from a _schema"
  [ztx group-name]
  (->> (get-in @ztx [:zd/meta])
       (filter (fn [[k v]]
                 (= group-name (:group v))))
       (map first)))

(defn append-meta
  "appends annotations from _schemas to a block"
  [ztx doc]
  (let [blocks-meta (:zd/meta @ztx)]
    (let [subdocs*
          (->> (:zd/subdocs doc)
               (map (fn [[subname cnt]]
                      [subname (append-meta ztx cnt)]))
               (into {}))]
      (->> doc
           (remove (fn [[k _]]
                     (namespace k)))
           (reduce (fn [*doc [k _]]
                     (update-in *doc [:zd/meta :ann k]
                                (fn [anns]
                                  (merge anns (get-in blocks-meta [k :ann])))))
                   (assoc doc :zd/subdocs subdocs*))))))

(defn load-meta!
  "load _schema into ztx"
  [ztx {c :content}]
  (let [ann-idx (reduce (fn [acc [k v]]
                          (->> (select-keys v [:type :schema :group :ann])
                               (assoc acc k)))
                        {}
                        (:zd/subdocs (reader/parse ztx {} c)))]
    (swap! ztx update :zd/meta merge ann-idx)))

(defn zen-schema
  "compile zen schema from _schema.zd"
  [ztx docname]
  (let [head {:type 'zen/map
              :validation-type :open}
        doc-keys (->> (:zd/meta @ztx)
                      (filter (fn [[_ v]]
                                (not= :subdoc (:type v))))
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
         :keys (->> (:zd/meta @ztx)
                    (filter (fn [[_ v]]
                              (= :subdoc (:type v))))
                    (map (fn [[k v]]
                           [k (:schema v)]))
                    (into {}))}]
    (-> {:zen/name 'zd.schema/document :tags #{'zen/schema}}
        (merge head)
        ;; TODO add :schema from top level of _schema.zd
;;        (merge (:schema doc-schema))
        (assoc-in [:keys :zd/meta] meta-sch)
        (assoc-in [:keys :zd/subdocs] subdocs)
        (update :keys merge doc-keys))))

(defn validate-doc
  "validate doc with zen schema compiled from a _schema.zd"
  [ztx doc]
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
