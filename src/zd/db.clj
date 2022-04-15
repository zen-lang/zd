(ns zd.db
  (:require
   [zen.core :as zen]
   [zd.parse]
   [sci.core]
   [clj-yaml.core :as yaml]
   [zd.deep-merge :refer [deep-merge]]
   [clojure.java.io :as io]
   [clojure.string :as str]))

#_(remove-ns 'zd.db)

(defn create-resource [ztx res]
  (swap! ztx assoc-in [:zdb (:zd/name res)] res))

(defmulti db-filter (fn [filter opts data] filter))

(defmethod db-filter :default
  [_ _ data]
  data)

(defmethod db-filter :namespace
  [_ ns data]
  (filterv (fn [{:zd/keys [name]}] (str/starts-with? name ns)) data))

(defmethod db-filter :select
  [_ keys data]
  (mapv #(select-keys % keys) data))

(defmethod db-filter :filter
  [_ {:keys [path value]} data]
  (filterv #(= (get-in % path) value)))

(defmethod db-filter :sort
  [_ sort data]
  (sort-by (fn [x] (str (get-in x sort))) data))

(defmethod db-filter :limit
  [_ n data]
  (take n data))

(defmethod db-filter :where
  [_ preds data]
  (let [where (apply every-pred preds)]
    (filterv where data)))

(defn apply-filters
  [filters data]
  (reduce-kv (fn [data filter opts] (db-filter filter opts data))
             data
             filters))

;; TODO: implement more filters
(defn search [ztx filter]
  (let [data (:zdb @ztx)]
    (->>  data
          (vals)
          (mapv (fn [x] (assoc (:resource x) :zd/name (:zd/name x))))
          (apply-filters filter))))

(defn select [ztx filter]
  (let [data (:zdb @ztx)]
    (->>  data
          (vals)
          (filterv filter)
          (take 100)
          (mapv :resource))))

(defn update-refs [ztx refs]
  (swap! ztx update :zrefs deep-merge refs))

(defn get-refs [ztx symbol]
  (get-in @ztx [:zrefs symbol]))

(defn group-refs-by-attr [ztx symbol]
  (let [refs (get-refs ztx symbol)
        distinct-attrs (->> refs
                            vals
                            (apply concat)
                            distinct)]
    (reduce
     (fn [acc attr]
       (assoc acc
              attr
              (reduce-kv
               (fn [acc res attrs]
                 (if (contains? attrs attr)
                   (conj acc res)
                   acc))
               []
               refs)))
     {} distinct-attrs)))


(defn index-refs [ztx]
  (reduce-kv
   (fn [acc res-name {zd-name :zd/name {title :title summary :summary} :resource}]
     (assoc acc res-name
            {:kpath zd-name
             :title (str/lower-case (str title))
             :summary (str/lower-case (str summary))}))
   {} (:zdb @ztx)))

(defn get-page [ztx nm]
  (get-in @ ztx [:zdb nm]))

(defn get-resource [ztx nm]
  (when-let [r (get-in @ ztx [:zdb nm :resource])]
    (assoc r :zd/name nm)))

(defn get-resources [ztx]
  (->> (get-in @ ztx [:zdb])
       (map (fn [[id {res :resource}]] (assoc res :zd/name id)))))

(defn get-doc [ztx nm]
  (get-in @ ztx [:zdb nm :doc]))


(defn *collect-refs [acc resource-name path node]
  (cond
    (symbol? node)
    (update-in acc [node resource-name] (fn [x] (conj (or x #{}) path)))

    (map? node)
    (->> node
         (reduce (fn [acc [k v]]
                   (*collect-refs acc resource-name (conj path k) v))
                 acc))

    (or (set? node) (sequential? node))
    (->> node
         (reduce (fn [acc v]
                   (*collect-refs acc resource-name (conj path :#) v))
                 acc))
    :else acc))

(defn collect-refs [resource-name resource]
  ;; {target {source #{[:path] [:path]}}}
  (*collect-refs {} resource-name [] resource))



(defn gather-parent-tags [ztx resource-name]
  (->> (str/split resource-name #"\.")
   (reduce (fn [doc-names nm-elem]
             (if (empty? doc-names)
               [nm-elem]
               (conj doc-names (str (last doc-names) "." nm-elem))))
           [])
   (mapv symbol)
   (reduce (fn [tags doc-sym]
             (clojure.set/union tags (:zd/child-tags (get-resource ztx doc-sym))))
           #{})))

(defn enrich-doc-with-annotations [ztx {doc :doc :as page}]
  (assoc
   page
   :doc
   (mapv
    (fn [block]
      (let [schema-pth (->> (:path block)
                            (mapv (fn [k]
                                    (cond
                                      (keyword? k) [:keys k]
                                      (number? k) :every
                                      :else (do
                                              (println (str "ERROR: unknown key '" k " in keypath (zd.db/enrich-doc-with-annotations)"))
                                              k))))
                            flatten)]
        (assoc block :annotations
               (merge
                (reduce
                 (fn [anns tag]
                   (merge anns (:zd/annotations (get-in (zen/get-symbol ztx tag) schema-pth))))
                 {}
                 (get-in page [:resource :zen/tags]))
                (:annotations block)))))
    doc)))

(def macros
  {'users (fn [& usrs] (->> usrs (mapv #(symbol (str "aidbox.team." %)))
                           (into #{})))
   'load (fn [ctx pth & [fmt]]
           (let [dir (->> (str/split (:zd/path ctx) #"/")
                          butlast
                          (str/join "/"))
                 content (slurp (str dir "/" pth))]
             (if (nil? content)
               ^:error {:message (str "File " pth " not found")}
               (cond
                 (= :yaml fmt) (yaml/parse-string content)
                 :else content))))})

(defn process-macroses [ztx {doc :doc :as page}]
  (assoc
   page
   :doc
   (mapv
    (fn [block]
      (if (and (list? (:data block)) (contains? macros (first (:data block))))
        (do
          (let [ctx (sci.core/init {:bindings macros})
                [fn-name & args] (:data block)
                form (-> args
                         (conj (assoc (dissoc block :data) :zd/path (:zd/path page) :zd/file (:zd/file page)))
                         (conj  fn-name))
                res (try
                      (sci.core/eval-form ctx form)
                         (catch Exception e
                           ^:error
                           {:message (.getMessage e)}))]
            (if (and (map? res) (:error (meta res)))
              (-> block
                  (assoc :data res)
                  (update :annotations merge {:block :error}))
              (assoc block :data res))))
        block))
    doc)))


(defn load-content! [ztx {:keys [resource-path path content]}]
  (let [resource-name (str/replace (str/replace resource-path #"\.zd$" "") #"/" ".")
        data (zd.parse/parse ztx content)
        parent-tags (gather-parent-tags ztx resource-name)
        tags (clojure.set/union (get-in data [:resource :zen/tags] #{}) parent-tags)
        namespaces (keep namespace tags)
        data (cond-> data
               (seq tags)
               (assoc-in [:resource :zen/tags] tags))
        _ (mapv #(zen/read-ns ztx (symbol %)) namespaces)
        data (assoc data :zd/name (symbol resource-name) :zd/file resource-path :zd/path path)
        data (enrich-doc-with-annotations ztx data)
        data (process-macroses ztx data)
        refs (collect-refs (symbol resource-name) (:resource data))
        errors (->> (:errors (zen/validate ztx (or (:zen/tags (:resource data)) #{}) (:resource data)))
                    (remove #(= "unknown-key" (:type %))))
        data (if (seq errors)
               (update data :doc conj {:path [:zd/errors] :annotations {:block :zen/errors} :data errors})
               data)]
    (create-resource ztx data)
    (update-refs ztx refs)))

(defn load-dirs [ztx dirs]
  (doseq [dir dirs]
    (let [dir (io/file dir)
          dir-path (.getPath dir)]
      (doseq [f (->> (file-seq dir)
                     (sort-by (fn [x] (.getPath x))))]
        (let [path (.getPath f)]
          (when (and (str/ends-with? path ".zd")
                     (not (str/starts-with? (.getName f) ".")))
            (let [resource-path (subs path (inc (count dir-path)))
                  content (slurp f)]
              (load-content! ztx {:path path
                                  :resource-path resource-path
                                  :content content}))))))))
