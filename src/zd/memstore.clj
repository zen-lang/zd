(ns zd.memstore
  (:require
   [zd.meta :as meta]
   [zd.methods :as methods]
   [zd.macros]
   [zen.core :as zen]
   [zen-web.utils :refer [deep-merge]]
   [zd.reader :as reader]
   [clojure.string :as str]))

(defn get-doc [ztx nm]
  (get-in @ztx [:zdb nm]))

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

(defn collect-links [ztx {{:keys [docname ann] :as meta} :zd/meta :as doc}]
  (->> (remove #(= "zd" (namespace (first %)))
               doc)
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
      (let [new-acc (reduce (fn [acc* [from path]]
                              (update-in acc* [to from] (fnil into #{}) path))
                            (update acc to dissoc (ffirst from))
                            from)]
        (recur new-acc oth)))))

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

(defn load-document! [ztx {:keys [root resource-path path content] :as doc}]
  (let [docname (meta/path->docname ztx resource-path)
        parent-link
        (->> (str/split (str docname) #"\.")
             (butlast)
             (str/join "."))
        doc-body {:zd/meta {:docname docname
                            :file resource-path
                            :ann {:parent {:zd/content-type :edn}}
                            ;; TODO add last updated from git to a document here?
                            :path path}
                  :parent
                  (cond
                    (= (str docname) root) ""
                    (str/blank? parent-link) (symbol root)
                    :else (symbol parent-link))}
        doc (->> content
                 (reader/parse ztx {})
                 (deep-merge doc-body)
                 (meta/append-meta ztx))
        links (collect-links ztx doc)
        macros (collect-macros ztx doc)]
    (swap! ztx assoc-in [:zdb docname] doc)
    (swap! ztx update :zrefs patch-links links)
    (swap! ztx update :zd/keys (fnil into #{}) (keys doc))
    (swap! ztx assoc-in [:zd/macros docname] macros)
    (zen/pub ztx 'zd.events/on-doc-load doc)))

(defn load-links!
  "add backlinks to docs"
  [ztx]
  (loop [[[docname links :as i] & oth] (:zrefs @ztx)]
    (cond
      (nil? i) 'ok

      ;; TODO hanging link is found, add to idx and display error
      (nil? (get-in @ztx [:zdb docname]))
      (recur oth)

      :else
      (let [links* (mapcat (fn [[from paths]]
                             (for [p paths]
                               ;; TODO rename :doc to :from
                               {:to docname :path p :doc from}))
                           links)]
        (swap! ztx assoc-in [:zdb docname :zd/meta :backlinks] (set links*))
        (recur oth)))))

(defn eval-macros! [ztx]
  (doseq [[sym macros] (get @ztx :zd/macros)]
    (doseq [[docpath macro] macros]
      (swap! ztx update-in [:zdb sym]
             (fn [doc]
               (-> doc
                   (assoc-in [:zd/meta :ann (first docpath) :zd/macro] macro)
                   (assoc (first docpath) (methods/eval-macro! ztx doc docpath macro))))))))
