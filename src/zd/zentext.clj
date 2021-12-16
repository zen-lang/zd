(ns zd.zentext
  (:require
   [clojure.string :as str]
   [sci.core]
   [stylo.core :refer [c]]
   [edamame.core]
   [clojure.java.io :as io])
  (:import [java.io StringReader]))

#_(remove-ns 'zd.zentext)


(def inline-regex #"(#[_a-zA-Z][-./a-zA-Z0-9]+|\[\[[^\]]+\]\]|\(\([^)]+\)\))")

(defn call-inline-link [s]
  [:a {:href (str "/" s) :class (c [:text :blue-600])} s])

(defmulti inline-method (fn [m arg] (keyword m)))

(defmethod inline-method
  :img
  [m arg]
  (let [[src alt] (str/split arg #"\s+" 2)]
    [:img {:src src :alt alt}]))

(defmethod inline-method
  :default
  [m arg]
  [:span {:class (c [:text :red-600] [:bg :red-100])} (str "No inline-method for " m " arg:" arg)])

(defn call-inline-method [s]
  (let [[method arg] (str/split s #"\s+" 2)]
    (inline-method method arg)))


(defmulti inline-function (fn [m arg] (keyword m)))

(defn call-inline-function [s]
  (try
    (let [[method arg] (str/split s #"\s+" 2)
          arg (edamame.core/parse-string arg)]
      (inline-function method arg))
    (catch Exception e
      [:error (pr-str e)])))

(defn parse-inline [ztx s]
  (let [m (re-matcher inline-regex s)
        res (loop [start 0
                   res []]
              (if (.find m)
                (let [head (subs s start (.start m))
                      match (subs s (.start m) (.end m))]
                  (recur
                   (.end m)
                   (conj res head (cond (str/starts-with? match "#")  (call-inline-link (subs match 1))
                                        (str/starts-with? match "[[") (call-inline-method   (subs match 2 (- (count match) 2)))
                                        (str/starts-with? match "((") (call-inline-function (subs match 2 (- (count match) 2)))))))
                (conj res (subs s start))))]
    (remove empty? res)))

(declare parse-block*)


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
           :eof  :end-ul
           :*    :end-ul}})

(defn get-token [l]
  (if l
    (cond
      (str/starts-with? l "```")    :block
      (or (str/starts-with? l "* ") (re-matches #"^\d+\) " l)) :list
      (or (re-matches #"\.+\* " l) (re-matches #"^\d+\) " l)) :sub-list
      (str/starts-with? l "..")    :sub-list
      (str/blank? l) :blank
      (= l :final) :final
      :else :text)
    :eof))

(defmulti apply-transition (fn [ztx action ctx line] action))

(defmethod apply-transition :nop [ztx tr ctx line] ctx)
(defmethod apply-transition :p-start
  [ztx _ ctx line]
  (assoc ctx :state :p :lines [line]))

(defmethod apply-transition :conj
  [ztx tr ctx line]
  (update ctx :lines conj line))

(defmethod apply-transition :p-end
  [ztx tr {lns :lines :as ctx} line]
  (-> (update ctx :result conj (into [:p] (mapcat (fn [l]
                                                    (parse-inline ztx l)) lns)))
      (assoc :state :none :lines [] :push-back true)))

(defmethod apply-transition :block-start
  [ztx _ ctx line]
  (assoc ctx
         :state :block
         :params line
         :lines []))

(defmulti process-block (fn [tp args cnt] tp))

(defmethod process-block "code" [_ lang cnt]
  [:pre [:code {:class (str "language-" lang " hljs")}
         cnt]])

(defmethod process-block :default [tp args cnt]
  [:block {:params args :tp tp} cnt])

(defmethod apply-transition :block-end
  [ztx _ {lns :lines params :params :as ctx} line]
  (let [block-params (str/split params #" " 2)
        tp (subs (first block-params) 3)
        args (second block-params)
        result (process-block tp args (str/join "\n" lns))]
    (-> (update ctx :result conj result #_(into [:block {:params params}] lns))
        (assoc :state :none :lines [] :params nil))))


(defmethod apply-transition :ul-start
  [ztx _ ctx line]
  (assoc ctx
         :state :ul
         :item line
         :items []
         :sub-items []))

(defn process-list-item [ztx ctx]
  (let [item (when (:item ctx)
               (parse-inline ztx (subs (:item ctx) 2)))]
    (cond-> [:li]
      item
      (into item)

      (seq (:sub-items ctx))
      (into (parse-block* ztx (mapv #(subs % 2) (:sub-items ctx)))))))

(defmethod apply-transition :ul-add
  [ztx _ ctx line]
  (-> ctx
      (assoc :items (conj (:items ctx) (process-list-item ztx ctx)))
      (assoc :item line)
      (assoc :sub-items [])))

(defmethod apply-transition :ul-add-sub
  [ztx _ ctx line]
  (-> ctx
      (assoc :sub-items (conj (:sub-items ctx) line))))

(defmethod apply-transition :end-ul
  [ztx _ ctx line]
  (-> (update ctx :result conj (into [:ul] (mapv (fn [x] x)
                                                 (conj (:items ctx)
                                                       (process-list-item ztx ctx)))))
      (assoc :state :none :items [] :sub-items [] :item nil)))

(defmethod apply-transition
  :default
  [ztx a ctx line]
  (println :missed a)
  ctx)

(defn parse-block* [ztx lines]
  (let [res (loop [[l & ls :as old-ls] lines
                   ctx  {:state :none :result []}]
              (let [token (get-token l)
                    action (or (get-in block-parser [(:state ctx) token])
                                   (get-in block-parser [(:state ctx) :*])
                                   {:action :unknown :state (:state ctx) :token token})
                    new-ctx (apply-transition ztx action ctx l)]
                ;; (println (:state ctx) token  :-> action :-> (dissoc new-ctx :result))
                (if (not= :eof token)
                  (if (:push-back new-ctx)
                    (recur old-ls (dissoc new-ctx :push-back))
                    (recur ls new-ctx))
                  (:result new-ctx))))]
    res))

(defn parse-block [ztx s]
  (let [lines (get-lines s)
        res (into [:div] (parse-block* ztx lines))]
    res))
