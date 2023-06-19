(ns zd.blocks.timeline
  (:require
   [zd.runner :as runner]
   [stylo.core :refer [c]]
   [zd.methods :as methods]
   [zd.link :as link]
   [clojure.string :as str]))

(defn create-timeline-data
  [[[_ author date] comment files]]
  (let [author (or author "")
        comment (or comment "")
        files (or files "")
        date (or date "")]
    {:comment (-> comment
                  first
                  clojure.string/trim)

     :user (-> author
               (clojure.string/split #"\s")
               second)

     :email (-> author
                (clojure.string/split #"\s")
                last
                rest
                butlast
                (clojure.string/join))

     :time (-> date
               (clojure.string/split #"\s")
               rest
               (->> (interpose " ")
                    (apply str)
                    (clojure.string/trim)))

     :files (vec files)}))

(defn get-history
  []
  (->> (runner/exec {:exec ["git" "log"
                            "--name-only"
                            "--date=format:%Y-%m-%d %H:%M"
                            "--no-merges"
                            "-n" "30"]})
       :stdout
       (partition-by empty?)
       (remove (fn [x] (-> x first empty?)))
       (partition 3)
       (mapv create-timeline-data)
       (group-by (fn [l] (first (str/split (:time l) #"\s" 2))))))

(defn gh-index [ztx]
  (->> (map second (:zdb @ztx))
       (filter (fn [x]
                 (str/starts-with? (name (get-in x [:zd/meta :docname])) "people.")))
       (reduce (fn [acc {ghn :git/names gh :github :as res}]
                 (if (or gh ghn)
                   (->> ghn
                        (reduce (fn [acc nm]
                                  (assoc acc nm res))
                                (assoc acc gh res)))
                   acc)))))

(defn gh-user [ztx gh-idx l]
  (if-let [u (get gh-idx (when-let [un (:user l)] (str/trim un)))]
    (link/symbol-link ztx (:zd/name u))
    [:b (or (:user l) (:email l))]))

(defmethod methods/renderkey :git/timeline
  [ztx ctx block]
  (let [gh-idx (gh-index ztx)]
    [:div
     (for [[date ls] (->> (get-history)
                          (sort-by first)
                          (reverse))]
       [:div
        [:div {:class (c :border-b :font-bold [:mt 2])} date]
        (for [l (reverse (sort-by :time ls))]
          [:div
           [:div {:class (c :flex :items-baseline [:space-x 2] [:ml 0] [:py 1])}
            [:div {:class (c [:text :gray-600])} (last (str/split (:time l) #"\s" 2))]
            [:div (gh-user ztx gh-idx l)]
            [:div (:comment l)]]
           [:ul {:class (c [:ml 6])}
            (->> (:files l)
                 (filter (fn [x] (str/starts-with? x "docs/")))
                 (map (fn [x] (symbol (-> (str/replace x #"(^docs/|\.zd$)" "")
                                          (str/replace #"/" ".")))))
                 (sort)
                 (mapv (fn [x] [:li (link/symbol-link ztx x)]))
                 (apply conj [:div]))]])])]))

