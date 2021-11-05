(ns zd.parser
  (:require
   [clojure.java.io :as io]
   [edamame.core]
   [zen.core :as zen]
   [clojure.string :as str])
  (:import [java.io StringReader]))


(defn str-reader [s]
  (io/reader (StringReader. s)))


(defn parse-value [k lns]
  (let [v (->> lns (filterv identity) (str/join "\n"))]
    (if (str/ends-with? (name k) ">")
      v
      (edamame.core/parse-string v))))


(defn parse [ztx md]
  (loop [res {}
         [l & ls] (line-seq (str-reader md))
         state :start
         current-key nil
         current-acc nil]
    (if (and (nil? l) (empty? ls))
      (if current-key
        (assoc res current-key (parse-value current-key current-acc))
        res)
      (if (str/starts-with? l ":")
        (let [res         (if current-key (assoc res current-key (parse-value current-key current-acc)) res)
              [a b]       (str/split l #"\s+" 2)
              current-key (keyword (subs a 1))
              current-acc [b]]
          (recur res ls :in-key current-key current-acc))
        (if (= :in-key state)
          (recur res ls state current-key (conj current-acc l))
          (recur (if (not (str/blank? l)) (update res :?> conj l) res) ls state current-key current-acc))))))

(defn name-to-path [ztx nm]
  (str (:zd/path @ztx) "/" (str/replace (name nm) #"\." "/") ".zd"))

(defn path-to-name [ztx p]
  (let [pth (get @ztx :zd/path)]
    (-> (subs p (inc (count pth)))
        (str/replace #"\.zd$" "")
        (str/replace #"/" ".")
        (symbol))))

(defn load-doc [ztx nm cnt]
  (swap! ztx assoc-in [:zd/resources nm] (parse ztx cnt)))

(defn load-file [ztx f]
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
  (read-doc ztx nm)
  (get-in @ztx [:zd/resources nm]))

(comment

  (def ztx (zen/new-context {:zd/path "zd"}))

  (read-doc ztx 'zd.features.format)




  )
