(ns zd.loader
  (:require
   [zen-web.utils :refer [deep-merge]]
   [zd.parser :as parser]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn get-doc [ztx nm]
  (get-in @ztx [:zdb nm]))

(defn get-blocks [ztx doc]
  (remove #(= "zd" (namespace (first %)))
          doc))

;; needed for windows compatibility
(def file-separator-regex
  (re-pattern
   (java.util.regex.Pattern/quote
    (System/getProperty "file.separator"))))

(defn *edn-links [acc docname path node]
  (cond
    (symbol? node)
    (update-in acc [node docname] (fnil conj #{}) path)

    (map? node)
    (reduce (fn [acc [k v]]
              (*edn-links acc docname (conj path k) v))
            acc
            node)

    (set? node)
    (reduce #(*edn-links %1 docname (conj path :#) %2)
            acc
            node)

    (and (sequential? node) (not (list? node)))
    (->> (map-indexed vector node)
         (reduce (fn [acc [idx v]]
                   (*edn-links acc docname (conj path idx) v))
                 acc))

    :else acc))

(defn edn-links [acc docname path cnt]
  (*edn-links acc docname path cnt))

(defn collect-links [ztx {:keys [zd/meta] :as doc}]
  (->> (get-blocks ztx doc)
       (reduce (fn [acc [k cnt]]
                 (let [cnt-type (get-in meta [:ann k :zd/content-type])]
                   (cond (= cnt-type :edn) (edn-links acc (:docname meta) [k] cnt)
                         :else acc)))
               {})))

(defn patch-links [idx patch]
  (loop [acc idx
         [[to from :as links] & oth] (seq patch)]
    (if (nil? links)
      acc
      (recur (reduce (fn [acc* [from path]]
                       (update-in acc* [to from] (fnil into #{}) path))
                     acc
                     from)
             oth))))

(defn load-document! [ztx {:keys [resource-path path content] :as doc}]
  (let [docname (symbol (str/replace (str/replace resource-path #"\.zd$" "")
                                     file-separator-regex
                                     "."))
        doc (deep-merge (parser/parse ztx {} content)
                        {:zd/meta {:docname docname
                                   :file resource-path
                                   :path path}})
        links (collect-links ztx doc)]
    (swap! ztx assoc-in [:zdb docname] doc)
    (swap! ztx update :zrefs patch-links links)))

(defn load-docs! [ztx dirs]
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
              (load-document! ztx {:path path
                                   :resource-path resource-path
                                   :content content}))))))))
