(ns zd.db
  (:require
   [zen.core :as zen]
   [zd.parse]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn deep-merge
  "efficient deep merge"
  ([a b & more]
   (apply deep-merge (deep-merge a b) more))
  ([a b]
   (loop [[[k v :as i] & ks] b
          acc a]
     (if (nil? i)
       acc
       (let [av (get a k)]
         (if (= v av)
           (recur ks acc)
           (recur ks
                  (cond
                    (and (map? v) (map? av)) (assoc acc k (deep-merge av v))
                    (and (nil? v) (map? av)) (assoc acc k av)
                    :else (assoc acc k v)))))))))

(defn create-resource [ztx res]
  (swap! ztx assoc-in [:zdb (:zd/name res)] res))

(defn update-refs [ztx refs]
  (swap! ztx update :zrefs deep-merge refs))

(defn get-refs [ztx symbol]
  (get-in @ztx [:zrefs symbol]))

(defn read-resource [ztx nm]
  (get-in @ ztx [:zdb nm]))

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
                  resource-name (symbol (str/replace (str/replace resource-path #"\.zd$" "") #"/" "."))
                  content (slurp f)
                  data (zd.parse/parse ztx content)
                  refs (collect-refs resource-name (:resource data))]
              (create-resource
               ztx (assoc data
                          :zd/name resource-name
                          :zd/file resource-path))
              (update-refs ztx refs))))))))
