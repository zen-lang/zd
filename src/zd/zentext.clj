(ns zd.zentext
  (:require
   [clojure.string :as str]
   [sci.core]
   [stylo.core :refer [c]]
   [edamame.core]
   [clojure.java.io :as io])
  (:import [java.io StringReader]))


(defn get-lines [s]
  (line-seq (io/reader (StringReader. s))))

(defn line-type [l]
  (when l
    (cond
      (str/starts-with? l "```") :block
      (or (str/starts-with? l "* ") (re-matches #"^\d+\) " l)) :list

      (str/blank? l) :blank
      :else :p)))

(defn action [state line-type]
  (cond
    (and (= :none state) (= :blank line-type)) :none
    (and (= :none state) (= :p line-type)) :p
    (and (= :p state)    (= :p line-type)) :p
    (and (= :p state)    (= :blank line-type)) :none
    (and (not (= state :block)) (= line-type :block)) :block
    (and (= state :block) (not (= :block line-type))) :block
    (and (= state :block) (= :block line-type)) :none
    (and (= line-type :list)) :list
    (and (= state :list) (contains? #{:block :blank} line-type)) :none
    :else :tbd))

(defn process-current-state [ctx]
  (cond
    (= :p (:state ctx))
    (into [:p] (:lines ctx))

    (= :block (:state ctx))
    (into [:pre] (:lines ctx))

    :else
    [:error ctx]))

(defn update-context [ctx old-state new-state l]
  ;; (println old-state new-state l)
  (cond
    (= [:none :p] [old-state new-state])
    (assoc ctx :lines [l] :state :p)

    (= [:p :p] [old-state new-state])
    (update ctx :lines (fn [x] (conj (or x []) l)))

    (= [:p :none] [old-state new-state])
    (-> ctx
        (update :res conj (process-current-state ctx))
        (assoc :state new-state :lines []))

    (and (not= :block old-state) (= :block  new-state))
    (-> ctx
        (update :res conj (process-current-state ctx))
        (assoc :state new-state
               :lines [(subs l 3)]))

    (= [:block :block] [old-state new-state])
    (update ctx :lines (fn [x] (conj (or x []) l)))

    (= [:block :none] [old-state new-state])
    (-> ctx
        (update :res conj (process-current-state ctx))
        (assoc :state :none :lines []))

    :else
    (assoc ctx :state new-state)))

(defn process-list [lines]
  (let [res
        (reduce
         (fn [ctx line]
           (let [parsed (re-find #"(\.+)?(\*|\d+\))\s+(.+)" line)
                 lvl (count (second parsed))
                 tp (if (= "*" (nth parsed 2)) :ul :ol)
                 line (nth parsed 3)
                 ctx (if (or (not= tp (:type ctx))
                             (not= lvl (:lvl ctx)))
                       (-> ctx
                           (assoc :type tp)
                           (assoc :lvl lvl)
                           (update :lists (fn [lists]
                                            (if (:list ctx)
                                              (conj lists (:list ctx))
                                              lists)))
                           (assoc :list [tp {:class (str "lvl-" lvl)}[:li line]]))
                       (update ctx :list #(conj % [:li line])))]
             ctx))
         {:lvl 0 :type nil :lists [:div]}
         lines)]
    (conj (:lists res) (:list res))))

(def block-parser
  {:none  {:block :block-start
           :blank :nop
           :text  :p-start
           :list  :ul-start}
   :p     {:text  :conj
           :*     :p-end}
   :block {:block :block-end 
           :*     :conj}
   :ul    {:list :ul-add
           :sub-list :ul-add-sub
           :*    :end-ul}})

(defn get-token [l]
  (when l
    (cond
      (str/starts-with? l "```")    :block
      (or (str/starts-with? l "* ") (re-matches #"^\d+\) " l)) :list
      (str/starts-with? l "..")    :sub-list
      (str/blank? l) :blank
      :else :text)))

(defmulti apply-transition (fn [action ctx line] action))

(defmethod apply-transition :nop [tr ctx line] ctx)
(defmethod apply-transition :p-start
  [_ ctx line]
  (assoc ctx :state :p :lines [line]))

(defmethod apply-transition :conj
  [tr ctx line]
  (update ctx :lines conj line))

(defmethod apply-transition :p-end
  [tr {lns :lines :as ctx} line]
  (-> (update ctx :result conj (into [:p] lns))
      (assoc :state :none :lines [] :push-back true)))

(defmethod apply-transition :block-start
  [_ ctx line]
  (assoc ctx
         :state :block
         :params line
         :lines []))

(defmethod apply-transition :block-end
  [_ {lns :lines params :params :as ctx} line]
  (-> (update ctx :result conj (into [:block {:params params}] lns))
      (assoc :state :none :lines [] :params nil)))


(defmethod apply-transition :ul-start
  [_ ctx line]
  (assoc ctx
         :state :ul
         :item line
         :items []))

(defmethod apply-transition :ul-add
  [_ ctx line]
  (println "Check subitems" (:sub-items line))
  (assoc ctx
         :item line
         :items (conj (:items ctx) (:item ctx))))

(defmethod apply-transition :ul-add-sub
  [_ ctx line]
  (assoc ctx :sub-items (conj (:sub-items ctx) line)))

(defmethod apply-transition :end-ul
  [_ ctx line]
  (-> (update ctx :result conj (into [:ul] (mapv (fn [x] [:li x]) (:items ctx))))
      (assoc :state :none :items nil)))


(defmethod apply-transition
  :default
  [a ctx line]
  (println :missed a)
  ctx)

(defn parse-block [ztx s]
  (let [lines (get-lines s)
        res (loop [[l & ls :as old-ls] lines
                   ctx  {:state :none :result [:div ]}]
              (let [token (get-token l)
                    action (or (get-in block-parser [(:state ctx) token])
                                   (get-in block-parser [(:state ctx) :*])
                                   {:action :unknown :state (:state ctx) :token token})
                    new-ctx (apply-transition action ctx l)]
                (println (:state ctx) token  :-> action :-> (dissoc new-ctx :result))
                (if l
                  (if (:push-back new-ctx)
                    (recur old-ls (dissoc new-ctx :push-back))
                    (recur ls new-ctx))
                  (:result ctx))))]
    (println res)))


