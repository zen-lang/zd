(ns zd.loader
  (:require
   [zd.methods :as methods]
   [zd.macros]
   [zen.core :as zen]
   [zen-web.utils :refer [deep-merge]]
   [zd.reader :as reader]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import [java.util Timer TimerTask]))

(declare blocks-meta)

  ;; TODO use turtle/json ld
(defn read-meta! [ztx]
  (def blocks-meta (read-string (slurp (io/resource "zd/blocks-meta.edn")))))

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
    (reduce #(*edn-links %1 docname path %2)
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

(def exclude #{\? \. \! \; \,})

(defn find-symbols [ch cnt]
  (loop [acc (seq cnt)
         syms #{}]
    (if (nil? acc)
      syms
      (let [[l r] (reader/split #(= % ch) acc)
            [le tail] (reader/split #(or (= % \space)
                                         (= % \newline))
                                    r)]
        (cond
          (nil? r) syms
          (not= (last l) \space) (recur tail syms)
          :else
          (let [sym (->> (rest le)
                         (reverse)
                         (drop-while #(contains? exclude %))
                         (reverse)
                         (apply str)
                         symbol)]
            (recur tail (conj syms sym))))))))

(defn zentext-links [acc docname path cnt]
  (let [links (find-symbols \# cnt)
        ;; zentext mention @ appends people.prefix
        mentions (map (fn [m] (symbol (str "people." m)))
                      (find-symbols \@ cnt))]

    (->> (into links mentions)
         (reduce (fn [acc* to]
                   (update-in acc* [to docname] (fnil conj #{}) path))
                 acc))))

(defn collect-links [ztx {{:keys [docname ann]} :zd/meta :as doc}]
  (->> (get-blocks ztx doc)
       (reduce (fn [acc [k cnt]]
                 (let [cnt-type (get-in ann [k :zd/content-type])]
                   (cond (= cnt-type :edn) (edn-links acc docname [k] cnt)
                         (= cnt-type :zentext) (zentext-links acc docname [k] cnt)
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

(defn *collect-macros [acc path node]
  (cond
    (and (list? node) (symbol? (first node)))
    (assoc acc path node)

    (map? node)
    (reduce (fn [acc [k v]]
              (*collect-macros acc (conj path k) v))
            acc
            node)

    ;; TODO add vector/seq traversal?
    :else acc))

(defn collect-macros [ztx {meta :zd/meta :as doc}]
  (->> doc
       (remove (fn [[k _]] (namespace k)))
       (filter (fn [[k _]] (= :edn (get-in meta [:ann k :zd/content-type]))))
       (reduce (fn [acc [k v]] (*collect-macros acc [k] v))
               {})))

(defn append-meta [ztx doc]
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
                              (fn [anns] (merge anns (get blocks-meta k)))))
                 (assoc doc :zd/subdocs subdocs*)))))

(defn load-document! [ztx {:keys [resource-path path content] :as doc}]
  ;; TODO add validation, annotations with zen.schema
  (let [docname (symbol (str/replace (str/replace resource-path #"\.zd$" "")
                                     file-separator-regex
                                     "."))
        headers {:zd/meta {:docname docname
                           :file resource-path
                           :path path}}
        doc (->> content
                 (reader/parse ztx {})
                 (deep-merge headers)
                 (append-meta ztx))
        links (collect-links ztx doc)
        macros (collect-macros ztx doc)]
    (swap! ztx assoc-in [:zdb docname] doc)
    (swap! ztx update :zrefs patch-links links)
    (swap! ztx update :zd/keys (fnil into #{}) (keys doc))
    (swap! ztx assoc-in [:zd/macros docname] macros)
    (zen/pub ztx 'zd.v2/on-doc-create doc)))

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

(defn unwrap-links
  "{to {from #{paths..}}} -> {from [[path to]..]}"
  [to links acc]
  (->> links
       (mapcat (fn [[from paths]]
                 (for [p paths]
                   ;; TODO rename :doc to :from
                   {:to to :path p :doc from})))
       (reduce (fn [acc {:keys [doc] :as link}]
                 (update acc doc conj link))
               acc)))

(defn load-links!
  "add links, invalid links to docs"
  [ztx]
  (loop [[[docname links] & oth] (:zrefs @ztx)]
    (cond
      (nil? docname) 'ok

      (nil? (get-in @ztx [:zdb docname]))
      (recur oth)

      :else
      (let [links* (mapcat (fn [[from paths]]
                             (for [p paths]
                               ;; TODO rename :doc to :from
                               {:to docname :path p :doc from}))
                           links)]
        (swap! ztx assoc-in [:zdb docname :zd/meta :backlinks] links*)
        (recur oth)))))

(defn eval-macros! [ztx]
  (let [eval-macro
        (fn [[sym macros]]
          (let [doc (get-in @ztx [:zdb sym])]
            (map (fn [[docpath macro]]
                   [sym docpath macro (methods/eval-macro! ztx doc docpath macro)])
                 macros)))]
    (->> (get @ztx :zd/macros)
         (mapcat eval-macro)
         (map (fn [[sym path macro result]]
                (swap! ztx update-in [:zdb sym]
                       (fn [doc]
                         (-> doc
                             (assoc-in [:zd/meta :ann (first path) :zd/macro] macro)
                             (assoc (first path) result))))))
         doall)))

(defn load-dirs! [ztx dirs]
  (load-docs! ztx dirs)
  (load-links! ztx)
  (eval-macros! ztx)
  'ok)

(defn hard-reload! [ztx dirs]
  (swap! ztx dissoc :zdb)
  (swap! ztx assoc :zrefs {})
  (read-meta! ztx)
  (zen/pub ztx 'zd/hard-reload {:dirs dirs})
  (load-dirs! ztx dirs))

(defn debounce
  ([f] (debounce f 300))
  ([f timeout]
   (let [timer (Timer.)
         task (atom nil)]
     (with-meta
       (fn [& args]
         (when-let [t ^TimerTask @task]
           (.cancel t))
         (let [new-task (proxy [TimerTask] []
                          (run []
                            (apply f args)
                            (reset! task nil)
                            (.purge timer)))]
           (reset! task new-task)
           (.schedule timer new-task timeout)))
       {:task-atom task}))))

(defn reload [ztx config]
  ;; TODO emit zen event
  (println :request-reload)
  (debounce #(hard-reload! ztx (:paths config)))
  :ok)