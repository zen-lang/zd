(ns zd.parse
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [zd.deep-merge :refer [deep-merge]]
   [zd.methods :refer [annotation]]
   [edamame.core])
  (:import [java.io StringReader]))

(defn get-lines [s]
  (line-seq (io/reader (StringReader. s))))

(defn comment? [l]
  (str/starts-with? l ";"))

(defn annotation? [l]
  (str/starts-with? l "^"))

(defn keypath? [l]
  (or
   (str/starts-with? l ":")
   (str/starts-with? l "~:")))

(re-matches #"^(:\S+)\s*(.*)$" ":key:kay {}")
(re-matches #"^(:\S+)\s*(\S*/)?(.*)$" ":key {}")
(re-matches #"^(~)?(:\S+)\s*(\S*/)?(.*)$" ":key md/ {}")

(defn to-path [s]
  (->> (str/split (str/replace s #"^(~:?|:)" "") #":")
       (mapv (fn [x]
               (if (re-matches #"\d+" x)
                 (Integer/parseInt x)
                 (keyword x))))))


(defn smart-assoc-in [m [k & ks] val]
  (try
    (if ks
      (assoc (if (int? k) (or m []) m) k (smart-assoc-in (get m k) ks val))
      (assoc (if (int? k) (or m []) m) k val))
    (catch Exception e
      (pr-str e))))

(defn parse-path [res l parent-path]
  (if-let [[_ child? path ann tail :as p] (re-matches #"^(~)?(:\S+)\s*([a-z]*/)?(.*)$" l)]
    {:child? (not (nil? child?))
     :path (if child?
             (into parent-path (to-path path))
             (to-path path))
     :annotation (cond
                   (and ann (= "/" (str/trim ann)))
                   {}
                   (and ann (not (str/blank? (str/trim (str/replace ann #"/$" "")))))
                   {:content (str/replace ann #"/$" "")})
     :line tail}
    {:status :error
     :line l}))


(defn parse-annotations [acc ann]
  (->> ann
       (reduce (fn [acc an]
                 (if (str/blank? an)
                   acc
                   (let [[k s] (str/split an #"\s" 2)
                         v (if (not (or (nil? s) (str/blank? s)))
                             (try (edamame.core/parse-string s)
                                  (catch Exception e
                                    (println :ERROR ann :- s)
                                    ;; (println  e)
                                    (str "Error: " (.getMessage e) " | " s)))
                             nil)]
                     (deep-merge acc (annotation (subs k 1) v)))))
               acc)))

(defn parse-keypath
  [{res :resource
    doc :doc
    ann :annotations
    kp :keypath
    lns :lines
    last-path :last-path
    :as ctx}]
  (if-not kp
    ctx
    (let [{p :path ch? :child? a :annotation l :line} (parse-path res kp last-path)
          lns (if l (into [l] lns) lns)
          content (str/trim (str/join "\n" lns))
          data (if a
                 content
                 (try (edamame.core/parse-string content)
                      (catch Exception e
                        (str "Error: " (.getMessage e) " | " content))))
          res-ann (parse-annotations (or a {}) ann)
          res (if (:view-only res-ann)
                res
                (smart-assoc-in res p data))]
      (-> ctx
          (update :doc conj (cond-> {:path p :data data}
                              (not (empty? res-ann))
                              (assoc :annotations res-ann)))
          (assoc :lines [] :annotations [] :keypath nil
                 :last-path (if ch? last-path p)
                 :resource res)))))

(def start-ctx
  {:resource {}
   :doc []
   :keypath nil
   :annotations []
   :last-path []
   :lines []})

(defn parse [ztx s]
  (let [lines (get-lines s)
        ctx (loop [ctx  start-ctx
                   [l & ls] lines]
              (cond
                (nil? l)
                (parse-keypath ctx)

                (comment? l)
                (recur ctx ls)

                (annotation? l)
                (-> (parse-keypath ctx)
                    (update :annotations conj l)
                    (recur ls))

                (keypath? l)
                (-> (parse-keypath ctx)
                    (assoc :keypath l)
                    (recur ls))

                :else
                (-> ctx
                    (update :lines conj l)
                    (recur ls))))]
    (select-keys ctx [:doc :resource])))
