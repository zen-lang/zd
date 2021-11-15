(ns zd.parser
  (:require
   [clojure.java.io :as io]
   [edamame.core]
   [zen.core :as zen]
   [clojure.string :as str])
  (:import [java.io StringReader]))


(defn str-reader [s]
  (io/reader (StringReader. s)))

(defn parse-value [path [first-line & lns :as all-lns]]
  (let [k (last path)]
    (if-let [format (and first-line (second (re-matches #"^\s*([a-zA-Z0-9]*)\s*/$" first-line)))]
      {:format format :content (->> lns (filterv identity) (str/join "\n"))}
      (let [v (->> all-lns (filterv identity) (str/join "\n"))]
        (try
          (edamame.core/parse-string v)
          (catch Exception e
            (println :parse-error v )
            (str "Error: " e)))))))

(defn add-link [ztx to from path]
  (swap! ztx assoc-in [:zd/links to path from] {}))

(defn collect-back-links [ztx nm path doc]
  (cond

    (map? doc)
    (doseq [[k v] doc]
      (collect-back-links ztx nm (conj path k) v))
    (sequential? doc)
    (->> doc
         (map-indexed (fn [k v]
                        (collect-back-links ztx nm (conj path k) v)))
         (doall))

    (set? doc)
    (doseq [v doc]
      (collect-back-links ztx nm path v))

    (symbol? doc)
    (add-link ztx doc nm path)))

(defn back-refs [ztx nm doc]
  (collect-back-links ztx nm [] (dissoc doc :zd/name)))

(defn smart-assoc-in [m [k & ks] val]
  (if ks
    (assoc (if (int? k) (or m []) m) k (smart-assoc-in (get m k) ks val))
    (assoc (if (int? k) (or m []) m) k val)))

(defn parse-path [res a parent-path]
  (let [pth (->> (str/split (str/replace a #"(^~|~$)" "") #"~")
                 (mapv (fn [x] (if (str/starts-with? x ":")
                                (keyword (subs x 1))
                                x))))
        pth (if (str/starts-with? a "~")
              (into (or parent-path []) pth)
              pth)]
    (loop [[p & ps] pth
           new-path []
           res res]
      (if (nil? p)
        new-path
        (if (= "#" p)
          (let [idx (cond (sequential? res) (count res)
                          (nil? res) 0
                          :else 0)]
            (recur ps (conj new-path idx) (get res idx)))
          (recur ps (conj new-path p) (get res p)))))))

(defn parse [ztx md]
  (loop [res {:zd/keys []}
         [l & ls] (line-seq (str-reader md))
         state :start
         parent-path nil
         current-path nil
         current-acc nil]
    (if (and (nil? l) (empty? ls))
      (if current-path
        (-> (assoc-in res current-path (parse-value current-path current-acc))
            (update :zd/keys conj current-path))
        res)
      (if (str/starts-with? l ";")
        (recur res ls state parent-path current-path current-acc)
        (if (or (str/starts-with? l ":") (str/starts-with? l "~"))
          (let [res         (if current-path
                              (-> (smart-assoc-in res current-path (parse-value current-path current-acc))
                                  (update :zd/keys conj current-path)) res)
                [a b]       (str/split l #"\s+" 2)
                current-path (parse-path res a parent-path)
                parent-path (if (str/ends-with? a "~")
                              current-path
                              parent-path)
                current-acc [b]]
            (if (str/ends-with? a "~")
              (recur
               (-> (smart-assoc-in res current-path (or (parse-value current-path current-acc) {}))
                   (update :zd/keys conj current-path))
               ls nil parent-path nil nil)
              (recur res ls :in-key parent-path current-path current-acc)))
          (if (= :in-key state)
            (recur res ls state parent-path current-path (conj current-acc l))
            (recur (if (not (str/blank? l)) (update res :?> conj l) res)
                   ls state parent-path current-path current-acc)))))))

(defn name-to-path [ztx nm]
  (str (:zd/path @ztx) "/" (str/replace nm #"\." "/") ".zd"))

(defn path-to-name [ztx p]
  (let [pth (get @ztx :zd/path)]
    (-> (subs p (inc (count pth)))
        (str/replace #"\.zd$" "")
        (str/replace #"/" ".")
        (symbol))))

(defn parent-tags [ztx nm]
  (let [parts (str/split (str nm) #"\.")]
    (loop [parts (butlast parts) tags #{}]
      (if (empty? parts)
        tags
        (let [parent-nm (symbol (str/join  "." parts))
              parent (get-in @ztx [:zd/resources parent-nm])
              parent-tags  (:zd/add-tags parent)]
          (recur (butlast parts)
                 (into tags parent-tags)))))))

(defn load-doc [ztx nm cnt & [props]]
  (let [doc (-> (parse ztx cnt)
                (assoc :zd/name nm)
                (merge props))
        tags (-> (into (or (:zd/tags doc) #{})
                       (parent-tags ztx nm))
                 (conj 'boxdoc/doc))
        _ (->> (mapv namespace tags)
               (into #{} )
               (mapv (fn [n] (zen.core/read-ns ztx n))))
        doc (let [{errs :errors} (zen/validate ztx tags (dissoc doc :zd/keys))]
              (if-not (empty? errs)
                (-> (assoc doc :zen/errors errs)
                    (update :zd/keys (fn [ks] (into [[:zen/errors]]  ks))))
                doc))]
    (swap! ztx assoc-in [:zd/resources nm] (assoc doc :zd/tags tags))
    (println :load nm)
    (back-refs ztx nm doc)))

(defn load-doc-file [ztx f]
  (let [p (.getPath f)]
    (when (and (str/ends-with? p ".zd")
               (not (str/starts-with? (.getName f) ".")))
      (when (.exists f)
        (load-doc ztx (path-to-name ztx p) (slurp f))))))

(defn read-doc [ztx nm]
  (let [pth (name-to-path ztx nm)
        f   (io/file pth)]
    (when(.exists f)
      (let [cnt (slurp f)]
        (load-doc ztx nm cnt)))))

(defn get-doc [ztx nm]
  (get-in @ztx [:zd/resources nm]))

(defn get-links [ztx nm]
  (get-in @ztx [:zd/links nm]))

(comment

  (def ztx (zen/new-context))

  (read-doc ztx 'zd.features.format)

  (parse ztx "
:title \"Title\"
:md>
My desc

:desc asci>

Here is ascidoc

:other markdown>

Here is markdown

:hiccup
[:div {:class (c [:w 4])}
 [:li ]
 ]

")



  )
