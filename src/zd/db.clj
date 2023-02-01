(ns zd.db
  (:require
   [clojure.set :as set]
   [matcho.core :as matcho]
   [zen.core :as zen]
   [zd.parse]
   [sci.core]
   [clojure.data.csv]
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

(defmethod db-filter :namespace/first
  [_ ns data]
  (filterv (fn [{:zd/keys [name]}] (and (str/starts-with? name ns) (re-find (re-matcher #"^[^.]+\.[^.]+$" (str name))))) data))

(defmethod db-filter :select
  [_ keys data]
  (mapv #(select-keys % keys) data))

(defmethod db-filter :filter
  [_ {:keys [path value]} data]
  (filterv #(= (get-in % path) value) data))

(defmethod db-filter :sort
  [_ sort data]
  (sort-by (fn [x] (str (get-in x sort))) data))

(defmethod db-filter :limit
  [_ n data]
  (take n data))

(defmethod db-filter :where
  [_ preds data]
  (let [where
        (->> preds
             (map (fn [p]
                    (let [expr (read-string (str/replace (str p) #"%" "document"))
                          fun (list 'fn '[document] expr)]
                      (fn [document]
                        (try (apply (eval fun) [document])
                             (catch Exception e false))))))
             (apply every-pred))]
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
  (->> (:zdb @ztx)
       (vals)
       (filter filter)
       (take 100)
       (mapv (fn [x] (assoc (:resource x) :zd/name (:zd/name x))))))

(defn data [ztx]
  (->> (:zdb @ztx)
        (vals)
        (map (fn [x] (assoc (:resource x) :zd/name (:zd/name x))))))

(defn update-refs [ztx refs]
  (swap! ztx update :zrefs deep-merge refs))

(defn update-macros [ztx macros]
  (swap! ztx update ::macros deep-merge macros))

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
  (when-let [p (get-in @ ztx [:zdb nm])]
    (assoc p :name nm)))

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

    (and (or (set? node) (sequential? node))
         (not (list? node)))
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

(defn format-csv
  [[header & content]]
  (let [header-keys (mapv keyword header)]
    (mapv
     (fn [raw]
       (zipmap header-keys raw))
     content)))

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
                 (= :csv fmt)  (-> content clojure.data.csv/read-csv format-csv) 
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

(defn collect-keypaths [ztx blocks]
  (->> blocks
       (mapv (fn [x]
               (when x
                 (swap! ztx update :zd/keys (fn [acc] (conj (or acc #{})
                                                           (str/join (:path x))))))))))

(defn *collect-macros [acc path docname node]
  (cond
    (and (list? node) (symbol? (first node)))
    (update acc docname assoc path node)

    (map? node)
    (reduce (fn [acc [k v]]
              (*collect-macros acc (conj path k) docname v))
            acc
            node)

    :else acc))

(defn collect-macros [{:keys [zd/name resource]}]
  (*collect-macros {} [] name resource))


(def file-separator-regex
  (re-pattern
   (java.util.regex.Pattern/quote 
    (System/getProperty "file.separator"))))


(defn load-content! [ztx {:keys [resource-path path content]}]
  (let [resource-name (str/replace (str/replace resource-path #"\.zd$" "") file-separator-regex ".")
        data (zd.parse/parse ztx content)
        parent-tags (gather-parent-tags ztx resource-name)
        tags (clojure.set/union (get-in data [:resource :zen/tags] #{}) parent-tags)
        data (cond-> data
               (seq tags)
               (assoc-in [:resource :zen/tags] tags))
        data (assoc data :zd/name (symbol resource-name) :zd/file resource-path :zd/path path)
        data (enrich-doc-with-annotations ztx data)
        data (process-macroses ztx data)
        macros (collect-macros data)
        refs (collect-refs (symbol resource-name) (:resource data))
        errors (->> (:errors (zen/validate ztx (or (:zen/tags (:resource data)) #{}) (:resource data)))
                    (remove #(= "unknown-key" (:type %))))
        data (if (seq errors)
               (update data :doc conj {:path [:zd/errors] :annotations {:block :zen/errors} :data errors})
               data)]
    (create-resource ztx data)
    (update-refs ztx refs)
    (update-macros ztx macros)
    (collect-keypaths ztx (:doc data))))

(defn invalid-refs->db [ztx invalid-idx]
  (doseq [[resource refs] invalid-idx]
    (let [errs (map (fn [[kp ref]]
                      {:refs ref :path kp :doc resource})
                    refs)]
      (swap! ztx update-in [:zdb resource]
             (fn [doc]
               (-> doc
                   (update :doc conj
                           {:path [:zd/broken-links]
                            :annotations {}
                            :data errs})
                   (update :resource assoc :zd/broken-links errs)))))))

(defn unwrap-refs [to-resource refs refs-idx]
  (->> refs
       (map (fn [[k v]]
              [k (reduce (fn [acc kp]
                           (assoc acc kp to-resource))
                         {}
                         v)]))
       (reduce (fn [acc [res new-kps]]
                 (update acc res (fn [kps]
                                   (reduce (fn [acc [kp ref]]
                                             (update acc kp (fnil conj #{}) ref))
                                           kps
                                           new-kps))))
               refs-idx)))

(defn backrefs->db [ztx resource-name refs]
  (swap! ztx update-in [:zdb resource-name]
         (fn [doc]
           (-> doc
               (update :resource assoc :zd/back-links refs)
               (update :doc conj {:path [:zd/back-links]
                                  :annotations {}
                                  :data refs})))))

(defn reset-zdb! [ztx]
  (swap! ztx assoc :zrefs {}))

(defn load-resources! [ztx dirs]
  (reset-zdb! ztx)
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

(defn infer-refs!
  "append backrefs and invalid refs to resources"
  [ztx]
  (loop [coll* (:zrefs @ztx)
         invalid-refs {}]
    (let [[resource-name refs] (first coll*)
          doc (get-in @ztx [:zdb resource-name])]
      (cond
        (nil? resource-name)
        (invalid-refs->db ztx invalid-refs)

        (nil? doc)
        (recur (rest coll*)
               (unwrap-refs resource-name refs invalid-refs))

        :else
        (do (backrefs->db ztx resource-name refs)
            (recur (rest coll*)
                   invalid-refs))))))

(defn matches? [resource pattern]
  (every? (fn [k]
            (let [l (get resource k)
                  r (get pattern k)]
              (and l
                   (cond
                     (and (set? l) (set? r))
                     (set/subset? r l)))))
          (keys pattern)))

(defn match-impl [ztx to-match pattern]
  (->> (:zdb @ztx)
       (filter (fn [[doc-name {:keys [resource] :as doc}]]
                 (and (not= doc-name (:zd/name to-match))
                      ;; TODO use matcho instead (fix set inclusion logic)
                      (matches? resource pattern))))
       (map first)
       set))

(defn query-doc [ztx {:keys [resource] :as doc} paths]
  (get-in resource (first paths)))

(defn query-impl [ztx doc expr]
  (cond
    (every? vector? expr)
    (query-doc ztx doc (rest expr))

    (= :* (first expr))
    (->> (:zdb @ztx)
         (mapcat (fn [[_ doc]]
                   (query-doc ztx doc (rest expr))))
         (filter identity))))

(defn eval-macros! [ztx]
  (let [eval-macro
        (fn [[sym cfg]]
          (let [doc (get-in @ztx [:zdb sym])]
            (doall
             (map (fn [[path macro]]
                    (let [[exp & args] macro]
                     ;; TODO commit zen.system version of hskb
                      (cond
                        (= exp 'query)
                        [sym path (query-impl ztx doc args)]

                        (= exp 'match)
                        [sym path (match-impl ztx doc (first args))])))
                  cfg))))

        append-mresult
        (fn [doc path v]
          (update doc :doc
                  (fn [blocks]
                    (map (fn [block]
                           (if (= (:path block) path)
                             (assoc block :data v)
                             block))
                         blocks))))]
    (doall
     (->> (get @ztx ::macros)
          (mapcat eval-macro)
          (filter vector?)
          (map (fn [[sym path result]]
                 (swap! ztx update-in [:zdb sym] append-mresult path result)))))))

(defn load-dirs [ztx dirs]
  (load-resources! ztx dirs)
  (infer-refs! ztx)
  (eval-macros! ztx))
