(ns zd.db
  (:require
   [zen.core :as zen]
   [zd.parse]
   [zd.deep-merge :refer [deep-merge]]
   [clojure.java.io :as io]
   [clojure.string :as str]))

#_(remove-ns 'zd.db)

(defn create-resource [ztx res]
  (swap! ztx assoc-in [:zdb (:zd/name res)] res))

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

(defn get-page [ztx nm]
  (get-in @ ztx [:zdb nm]))

(defn get-resource [ztx nm]
  (get-in @ ztx [:zdb nm :resource]))

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

(defn load-content! [ztx path content]
  (let [resource-name (str/replace (str/replace path #"\.zd$" "") #"/" ".")
        data (zd.parse/parse ztx content)
        parent-tags (gather-parent-tags ztx resource-name)
        tags (clojure.set/union (get-in data [:resource :zen/tags] #{}) parent-tags)
        namespaces (keep namespace tags)
        data (cond-> data
               (seq tags)
               (assoc-in [:resource :zen/tags] tags))
        _ (mapv #(zen/read-ns ztx (symbol %)) namespaces)
        refs (collect-refs (symbol resource-name) (:resource data))
        errors (->> (:errors (zen/validate ztx (conj (or (:zen/tags (:resource data)) #{}) 'zen/any) (:resource data)))
                    (remove #(= "unknown-key" (:type %))))]
    (create-resource
     ztx (cond-> data
           true
           (assoc :zd/name (symbol resource-name)
                  :zd/file path)
           (seq errors)
           (assoc :errors errors)))
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
              (load-content! ztx resource-path content))))))))
