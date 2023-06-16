(ns zd.meta
  (:require
   [zen-web.utils :as utils]
   [zd.reader :as reader]
   [clojure.string :as str]
   [zen.core :as zen]))

(defn get-parent [ztx nm]
  (when-let [p (seq (butlast (str/split (str nm) #"\.")))]
    (symbol (str/join "." p))))

;; needed for windows compatibility
(def file-separator-regex
  (re-pattern
   (java.util.regex.Pattern/quote
    (System/getProperty "file.separator"))))

(defn path->docname [ztx resource-path]
  (-> resource-path
      (str/replace #"\.zd$" "")
      (str/replace file-separator-regex ".")
      (symbol)))

(defn get-group
  "finds keys by :group from a _schema"
  [ztx group-name]
  (->> (get-in @ztx [:zd/schema :keys-idx])
       (filter (fn [[k v]]
                 (= group-name (:group v))))
       (map first)))

(defn append-meta
  "appends annotations from _schema's to a key"
  [ztx doc]
  (let [keys-meta (get-in @ztx [:zd/schema :keys-idx])]
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
                                  (merge anns (get-in keys-meta [k :ann])))))
                   (assoc doc :zd/subdocs subdocs*))))))

(defn load-meta!
  "load _schema into ztx"
  [ztx {c :content rp :resource-path}]
  (let [sch (reader/parse ztx {} c)
        keys-ns (or (get-parent ztx (path->docname ztx rp)) :zd/root)
        keys-idx (reduce (fn [acc [k v]]
                           (-> acc
                               (assoc k (select-keys v [:type :schema :group :ann]))
                               (assoc-in [k :namespace] keys-ns)))
                         {}
                         (:zd/subdocs sch))]
    (swap! ztx update :zd/schema
           (fn [st]
             (cond-> st
               (:schema sch) (update-in [:schemas keys-ns] merge (:schema sch))
               :always (update :keys-idx merge keys-idx))))))

(defn zen-schema
  "compile zen schema for a document from _schema's defined in knowledge base"
  [ztx docname]
  (let [doc-keys (->> (:keys-idx (:zd/schema @ztx))
                      (filter (fn [[_ v]]
                                (and (not= :subdoc (:type v))
                                     (or (= (:namespace v) :zd/root)
                                         (str/includes? (str docname) (str (:namespace v)))))))
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
         ;; TODO enable validation of nested subdocs /w zen/schema :confirms
         :values {:type 'zen/map
                  :validation-type :open
                  :keys (merge doc-keys {:zd/meta meta-sch})}
         :keys (->> (:keys-idx (:zd/schema @ztx))
                    (filter (fn [[_ v]]
                              (= :subdoc (:type v))))
                    (map (fn [[k v]]
                           [k (:schema v)]))
                    (into {}))}
        toplevel
        (utils/deep-merge (get-in @ztx [:zd/schema :schemas (get-parent ztx docname)])
                          (get-in @ztx [:zd/schema :schemas :zd/root]))]

    (-> {:zen/name 'zd.schema/document
         :tags #{'zen/schema}
         :type 'zen/map
         :validation-type :open}
        (merge toplevel)
        (assoc-in [:keys :zd/meta] meta-sch)
        (assoc-in [:keys :zd/subdocs] subdocs)
        (update :keys merge doc-keys))))

(defn validate-doc
  "validate doc with zen schema compiled from a _schema.zd"
  [ztx doc]
  (let [docname (str (:zd/docname doc))
        sch (zen-schema ztx docname)
        errs (->> (zen/validate-schema ztx sch doc)
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
                        :message "Rename :zd/docname from _draft"})))))

(defn annotations
  "list annotations used in _schema"
  [ztx]
  ;; TODO think about defining annotations in meta ?
  (->> (vals (:keys-idx (:zd/schema @ztx)))
       (map :ann)
       (mapcat (fn [el]
                 (when (map? el)
                   (keys el))))
       (set)
       (map (fn [e] {:name (str "^" (name e))}))))
