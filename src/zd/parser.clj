;; new parser
(ns zd.parser
  (:require
   [clojure.string :as str]
   [edamame.core]
   [clojure.java.io :as io])
  (:import [java.io StringReader]))


(defn is-comment? [l]
  (str/starts-with? l ";"))

(defn is-ann? [l]
  (str/starts-with? l "^"))

(defn is-key? [l]
  (str/starts-with? l ":"))

(defn is-res? [l]
  (str/starts-with? l "&"))

(defn get-lines [s]
  (line-seq (io/reader (StringReader. s))))

;; while l
;; case
;;  ^ -> annotation to stack (till ^ : #)
;;  : -> start key (till next : or ^ or #)
;;  # -> start resource (till next #)

;; [{:zd/name .} {:zd/name .sub} {:zd/name .sub}]

(defn collect [l lines]
  (if (empty? lines)
    [[l] []]
    (loop [acc [l] [l & lns :as prev-lns] lines]
      (if (and (nil? l) (empty? lns))
        [acc prev-lns]
        (if (or (is-ann? l) (is-key? l) (is-res? l))
          [acc prev-lns]
          (recur (conj acc l) lns))))))

;; ^ann -> start annotation
;; ;; get all lines unless ann, key or res

;; :key ->
;; ;; get all lines unless ann, key or res


(defn parse-ann [ctx ls])
(defn parse-key [ctx ls])

(defn line-type [l]
  (cond
    (is-key? l) :key
    (is-ann? l) :ann
    (is-res? l) :res
    :else :other))

(defn start-ann [ctx l]
  (assoc ctx :state :ann :lines [l]))

(defn add-line [ctx l]
  (update ctx :lines (fn [x] (conj (or x []) l))))

(defmulti add-node (fn [ltype ctx acc] ltype))

(defn parse-value [l ls]
  (if (re-matches #"^\s*\|.*" l)
    [(str/trim (->> (into [(second (str/split l #"\|" 2))] ls)
                    (str/join "\n")
                    (str/trim)))]
    (if (re-matches #"^\s*/.*" l)
      [(->> (into [(second (str/split l #"/" 2))] ls)
            (str/join "\n")
            (str/trim)) :zentex]
      [(try
         (edamame.core/parse-string (str/join "\n" (into [l] ls)))
         (catch Exception e e))
       :edn])))


(defmethod add-node
  :key
  [_ ctx [l & ls]]
  (-> ctx
      (update :resource
              (fn [res]
                (let [[k val] (str/split l #"\s" 2)
                      orig-val (str/join "\n" (into [val] ls))
                      [val ann] (parse-value val ls)
                      k (keyword (subs k 1))]
                  (-> (assoc res k val)
                      (assoc-in [:zd/meta :text-values k] orig-val)
                      (assoc-in [:zd/meta :current-values k] val)
                      (update-in [:zd/meta :doc] conj k)
                      (cond-> ann
                        (assoc-in [:zd/meta :ann k ann] {}))))))))

(defn parse [ztx ctx text]
  (loop [ctx {:resource {:zd/meta {:doc [] :ann {} :text-values {}}}}
         [l & ls] (get-lines text)]
    (if (nil? l)
      ctx
      (let [ltype (line-type l)]
        (if (= ltype :other)
          (throw (Exception. "ups"))
          (let [[acc ls] (collect l ls)]
            (recur (add-node ltype ctx acc) ls)))))))

(comment


  (parse (atom {}) {} ":key | value \n:key | other val")

  (parse (atom {}) {} ":key 1")
  (parse (atom {}) {} ":key 1 ")
  (parse (atom {}) {} ":key [1 2 3]")
  (parse (atom {}) {} ":key ")
  (parse (atom {}) {} "^datalog\n:key {:where [[e :key something] [e :type Chat]]}")

  (parse (atom {}) {} ":key (load \"file.csv\")")
  (parse (atom {}) {} ":key / \nsome\ntext\n")
  (parse (atom {}) {} ":one 1\n:two \"two\"\n:desc /\n* one\n*two")

  )




(defn unparse [ztx ctx resource]

  )
