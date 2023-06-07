(ns zd.reader
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

(defmulti parse! (fn [ztx ctx l acc] (btype (or l ""))))

(defmulti collect! (fn [ztx ctx l acc] (btype (or l ""))))

(defn get-lines [s]
  (line-seq (io/reader (StringReader. s))))

(defn parse* [ztx ctx text]
  ;; TODO add comments into text values so unparse works
  (loop [ctx {:zd/meta {:doc [] :errors [] :ann {} :text-values {}}}
         [l & ls] text]
    (cond (nil? l) ctx
          (str/blank? l) (recur ctx ls)
            ;; TODO :zd/unparse add comment to resource as block
          (is-comment? l) (recur ctx ls)

          (is-block? l) (let [[acc ls] (collect! ztx ctx l ls)]
                          (recur (parse! ztx ctx l acc) ls))

          :else (recur ctx ls)
          ;; TODO decide if loose behaviour is better
          #_(update-in ctx [:zd/meta :errors]
                           conj {:type :zd.reader/unknown-line
                                 :message (str "error reading line: " l)
                                 :path []}))))

(defn parse [ztx ctx text]
  (parse* ztx ctx (get-lines text)))

(defn split [pred coll & filters]
  (loop [left [] [n & coll*] coll]
    (cond (nil? n) (list (seq left) nil)
          (some #(% n) filters) (recur left coll*)
          (pred n) (list (seq left) (conj coll* n))
          :else (recur (conj left n) coll*))))

(defmethod collect! :ann
  [ztx doc l lines]
  (let [[_ head] (split is-key? lines)]
    (if (nil? head)
      ;; TODO add zd/error
      [head (rest lines)]
      (let [[cnt tail] (collect! ztx doc (first head) (rest head))]
       [(conj cnt (first head)) tail]))))

(defmethod collect! :doc
  [ztx _ l lines]
  (let [[cnt tail] (split is-doc? lines is-comment?)]
    [cnt tail]))

(defn saferead [v]
  (try (reader/parse-string v)
       (catch Exception e
         ;; TODO emit event
         (prn ":saferead parsing warning"))))

(defn collect-edn [ztx doc lines]
  (loop [[f & tail :as ls] lines
         acc []]
    (cond (nil? ls) [nil lines]
          (str/blank? f) (recur tail acc)
          (and (is-block? f) (empty? acc)) [nil ls]
          :else
          (if (try (->> (conj acc f)
                        (apply str)
                        (reader/parse-string))
                   (catch Exception _))
            [(seq (conj acc f)) tail]
            (recur tail (conj acc f))))))

(defmethod collect! :key
  [ztx doc l lines]
  (let [cnt-type (->> (str/trim l)
                      (reverse)
                      (rest)
                      (take-while #(not= % \space))
                      (reverse)
                      (apply str)
                      keyword)

        multiline-edn? (and (= \/ (last l))
                            (or (= :datalog cnt-type)
                                (= :edn cnt-type)))]
    (if multiline-edn?
      (let [[cnt tail] (collect-edn ztx doc lines)]
        ;; TODO add error to :zd/errors
        [cnt tail])
      (let [[cnt tail] (split is-block? lines is-comment?)]
        [cnt tail]))))

(defmethod parse! :doc
  [ztx ctx l ls]
  (let [dot? (fn [ch] (= ch \.))
        docpath
        (if (dot? (first (rest l)))
          (->> (rest l)
               (partition-by dot?)
               (remove #(dot? (first %)))
               (map #(apply str %))
               (mapv keyword))
          [(keyword (apply str (rest l)))])]
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
  [ztx ctx ann ls]
  (let [key (->> (first ls)
                 (take-while #(not= % \space))
                 (rest)
                 (apply str)
                 (keyword))
        [k val] (split #(= % \space) (rest ann))
        ann (keyword (apply str k))]
    (assoc-in (parse! ztx ctx (first ls) (rest ls))
              ;; TODO add error if ann is not parsed correctly
              [:zd/meta :ann key ann] (saferead (apply str val)))))

(defmethod parse! :key
  [ztx ctx l ls]
  (let [[k val] (split #(= % \space) (str/trim l))
        multiline? (= \/ (last val))
        k (keyword (apply str (rest k)))
        cnt-type (if multiline?
                   (let [ann (str/trim (apply str (butlast val)))]
                     (if (str/blank? ann)
                       :zentext
                       (keyword ann)))
                   :edn)
        lines (if multiline?
                (apply str ls)
                (apply str val))
        cnt (cond (str/blank? lines) lines
                  (= cnt-type :edn) (saferead lines)
                  (= cnt-type :datalog) (saferead lines)
                  (= cnt-type :zentext) (str/join "\n" ls))
        ctx* (-> ctx
                 (update-in [:zd/meta :doc] conj k)
                 (assoc-in [:zd/meta :ann k :zd/content-type] cnt-type)
                 (assoc-in [:zd/meta :ann k :zd/multiline] multiline?)
                 (assoc k (or cnt lines)))]
    (cond-> ctx*
      (and (nil? cnt) (or (= cnt-type :edn) (= cnt-type :datalog)))
      (update-in [:zd/meta :errors] conj {:type :edn-reading-error
                                          :path [k]
                                          :message "Invalid edn format"}))))

(defmethod parse! :default
  [ztx ctx l lines]
  (update-in ctx [:zd/meta :errors] conj
             {:type :block-undefined
              :message "can not parse block"
              :lines (conj lines l)}))
