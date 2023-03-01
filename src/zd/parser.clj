(ns zd.parser
  (:require
   [clojure.string :as str]
   [edamame.core :as reader]
   [clojure.java.io :as io])
  (:import [java.io StringReader]))

(defn is-comment? [l]
  (str/starts-with? l ";"))

(defn is-ann? [l]
  (str/starts-with? l "^"))

(defn is-key? [l]
  (str/starts-with? l ":"))

(defn is-doc? [l]
  (str/starts-with? l "&"))

(defn is-block? [l]
  (or (is-ann? l) (is-key? l) (is-doc? l)))

(defn btype [l]
  (cond
    (is-ann? l) :ann
    (is-key? l) :key
    (is-doc? l) :doc))

(defmulti parse! (fn [ztx ctx l acc] (btype l)))

(defmulti collect! (fn [ztx ctx l acc] (btype l)))

(defn get-lines [s]
  (line-seq (io/reader (StringReader. s))))

(defn parse* [ztx ctx text]
  (loop [ctx {:zd/meta {:doc [] :ann {} :text-values {}}}
         [l & ls :as lines] text]
    (cond (nil? l) ctx
          (str/blank? l) (recur ctx ls)
            ;; TODO :zd/unparse add comment to resource as block
          (is-comment? l) (recur ctx ls)

          (is-block? l) (let [[acc ls] (collect! ztx ctx l ls)]
                          (recur (parse! ztx ctx l acc) ls))

          :else (update ctx :zd/errors conj {:type :unknown-line :value l}))))

(defn parse [ztx ctx text]
  (parse* ztx ctx (get-lines text)))

(defn split [pred coll & filters]
  (loop [left [] [n & coll*] coll]
    (cond (nil? n) (list (seq left) nil)
          (some #(% n) filters) (recur left coll*)
          (pred n) (list (seq left) (conj coll* n))
          :else (recur (conj left n) coll*))))

(defmethod collect! :ann
  [ztx _ l lines]
  (let [[_ head] (split is-key? lines)
        [cnt tail] (split is-block? (rest head) is-comment?)]
    [(concat [l (first head)] cnt) tail]))

(defmethod collect! :doc
  [ztx _ l lines]
  (let [[cnt tail] (split is-doc? lines is-comment?)]
    [(conj cnt l) tail]))

(defmethod collect! :key
  [ztx _ l lines]
  (let [[cnt tail] (split is-block? lines is-comment?)]
    [(conj cnt l) tail]))

(defn saferead [v]
  (try (or (reader/parse-string v) {})
       (catch Exception e
         ;; TODO emit event
         (do (prn ":parsing error " e) v))))

(defmethod parse! :doc
  [ztx ctx _ [doc & ls]]
  (let [dot? (fn [ch] (= ch \.))
        docpath
        (if (dot? (first (rest doc)))
          (->> (rest doc)
               (partition-by dot?)
               (remove #(dot? (first %)))
               (map #(apply str %))
               (mapv keyword))
          [(keyword (apply str (rest doc)))])]
    (loop [[p & ps :as dp] docpath
           curpath []
           ctx* ctx]
      (if (empty? dp)
        ctx*
        (let [prefix (interleave (repeat :zd/subdocs) curpath)]
          (recur (rest dp) (conj curpath p)
                 (-> ctx
                     (update-in (concat prefix [:zd/meta :doc]) conj p)
                     (assoc-in (concat prefix [:zd/subdocs p]) (parse* ztx ctx* ls)))))))))

(defmethod parse! :ann
  [ztx ctx _ [ann & ls]]
  (let [key (->> (first ls)
                 (take-while #(not= % \space))
                 (rest)
                 (apply str)
                 keyword)
        [k val] (split #(= % \space) (rest ann))
        ann (keyword (apply str k))]
    (assoc-in (parse! ztx ctx (first ls) ls)
              [:zd/meta :ann key ann] (saferead (apply str val)))))

(defmethod parse! :key
  [ztx ctx _ [l & ls]]
  (let [[k val] (split #(= % \space) l)
        multiline? (= \/ (last val))
        key (keyword (apply str (rest k)))
        cnt-type (if multiline?
                   (let [ann (str/trim (apply str (butlast val)))]
                     (if (str/blank? ann)
                       :zentext
                       (keyword ann)))
                   :edn)
        ctx* (-> ctx
                 (update-in [:zd/meta :doc] conj key)
                 (assoc-in [:zd/meta :ann key :zd/content-type] cnt-type))]

    (cond (= :edn cnt-type)
          (assoc ctx* key
                 (saferead (if multiline?
                             (apply str ls)
                             (apply str val))))

          (= :zentext cnt-type)
          (assoc ctx* key (str/join "\n" ls))

          :else
          (assoc ctx* key ls))))
