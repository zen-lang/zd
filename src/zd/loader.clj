(ns zd.loader
  (:require
   [zd.fs :as fs]
   [zd.methods :as methods]
   [zd.utils :as utils]
   [zd.macros]
   [zen.core :as zen]
   [zen-web.utils :refer [deep-merge]]
   [zd.reader :as reader]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import [java.util Timer TimerTask]))

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
  (let [blocks-meta (:zd/schema @ztx)]
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

(defn load-document! [ztx {:keys [resource-path path content] :as doc}]
  (let [docname (symbol (str/replace (str/replace resource-path #"\.zd$" "")
                                     file-separator-regex
                                     "."))
        headers {:zd/meta {:docname docname
                           :file resource-path
                           :last-updated (zen/op-call ztx 'zd/gitsync-last-updated path)
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
    (zen/pub ztx 'zd/on-doc-create doc)))

(defn load-meta! [ztx {c :content}]
  (let [ann-idx (reduce (fn [acc [k v]]
                          (->> (select-keys v [:type :schema :group :ann])
                               (assoc acc k)))
                        {}
                        (:zd/subdocs (reader/parse ztx {} c)))]
    (swap! ztx update :zd/schema merge ann-idx)))

(defn load-docs! [ztx dirs]
  (doseq [dir dirs]
    (let [dir (io/file dir)
          dir-path (.getPath dir)]
      ;; load metadata
      (doseq [f (->> (file-seq dir)
                     (filter (fn [f] (str/includes? (.getName f) "_schema.zd"))))]
        (let [content (slurp f)]
          (load-meta! ztx {:path (.getPath f)
                           :resource-path (subs (.getPath f) (inc (count dir-path)))
                           :content content})))
      ;; load documents
      (doseq [[path f] (->> (file-seq dir)
                            (map (fn [d] [(.getPath d) d]))
                            (sort-by first))]
        (when (and (str/ends-with? path ".zd")
                   (not (str/starts-with? (.getName f) ".")))
          (let [resource-path (subs path (inc (count dir-path)))
                content (slurp f)]
            (load-document! ztx {:path path
                                 :resource-path resource-path
                                 :content content})))))))

(defn load-links!
  "add backlinks to docs"
  [ztx]
  (loop [[[docname links :as i] & oth] (:zrefs @ztx)]
    (cond
      (nil? i) 'ok

      ;; hanging link is found
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

;; first arg is an agent state
(defn hard-reload! [_ ztx {dirs :paths :as config}]
  ;; TODO emit zen event
  (prn :hard-reload)
  (swap! ztx dissoc :zdb :zd/schema)
  (swap! ztx assoc :zrefs {})
  (zen/pub ztx 'zd/hard-reload {:dirs dirs})
  (load-dirs! ztx dirs))

(defonce ag (agent nil))

(defn reload! [ztx]
  ;; TODO emit zen event
  (println :request-reload)
  ;; TODO do we need to do discovery here?
  ;; maybe provide config explicitly?
  (let [config (->> (zen/get-tag ztx 'zd/config)
                    (first)
                    (zen/get-symbol ztx))
        sf (utils/safecall hard-reload! {:type :zd.loader/reload-error})]
    (send-off ag sf ztx config)))

(defn reload-sync! [ztx]
  (reload! ztx)
  (await ag)
  (deref ag))
