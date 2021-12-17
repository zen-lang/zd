(ns zd.zentext
  (:require
   [clojure.string :as str]
   [sci.core]
   [zd.db]
   [stylo.core :refer [c]]
   [edamame.core]
   [clojure.java.io :as io])
  (:import [java.io StringReader]))

#_(remove-ns 'zd.zentext)

(def inline-regex #"(#[_a-zA-Z][-./a-zA-Z0-9]+|\[\[[^\]]+\]\]|\(\([^)]+\)\))")

(defn call-inline-link [ztx s]
  (if-let [res (zd.db/get-resource ztx (symbol s))]
    [:a {:href (str "/" s) :class (c [:text :blue-600])} s]
    [:a {:href (str "/" s) :class (c [:text :red-600] [:bg :red-100]) :title "Broken Link"} s]))

(defmulti inline-method (fn [m arg] (keyword m)))

(defmethod inline-method
  :img
  [m arg]
  (let [[src alt] (str/split arg #"\s+" 2)]
    [:img {:src src :alt alt}]))

(defmethod inline-method
  :src
  [m arg]
  [:a {:class (c [:text :green-500])} (str "Code :" arg)])

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
                   (conj res head (cond (str/starts-with? match "#")  (call-inline-link ztx  (subs match 1))
                                        (str/starts-with? match "[[") (call-inline-method   (subs match 2 (- (count match) 2)))
                                        (str/starts-with? match "((") (call-inline-function (subs match 2 (- (count match) 2)))))))
                (conj res (subs s start))))]
    (remove empty? res)))

(declare parse-block*)


(defn get-lines [s]
  (line-seq (io/reader (StringReader. s))))

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
           :list  :list-start}
   :p     {:text  :conj
           :*     :p-end}
   :block {:block :block-end 
           :*     :conj}
   :ol {:list :list-add-elem
        :sub-list :list-add-sub-elem
        :*    :end-list}
   :ul {:list :list-add-elem
        :sub-list :list-add-sub-elem
        :*    :end-list}})

(defn get-token [l]
  (if l
    (cond
      (str/starts-with? l "```") :block
      (or (re-matches #"^\* .+" l) (re-matches #"^\d+\) .+" l)) :list
      (or (re-matches #"^\.+\* " l) (re-matches #"^\.+\d+\) " l)) :sub-list
      (str/starts-with? l "..") :sub-list
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


(defmethod apply-transition :list-start
  [ztx _ ctx line]
  (assoc ctx
         :state (if (str/starts-with? line "*") :ul :ol)
         :item line
         :items []
         :sub-items []))


(defn process-list-item [ztx ctx]
  (let [item (when (:item ctx)
               (parse-inline ztx (last (re-find #"^(\* |\d+\) )(.+)" (:item ctx)))))]
    (cond-> [:li]
      item
      (into item)

      (seq (:sub-items ctx))
      (into (parse-block* ztx (mapv #(subs % 2) (:sub-items ctx)))))))

(defmethod apply-transition :list-add-elem
  [ztx _ ctx line]
  (if (= (:state ctx) (if (str/starts-with? line "*") :ul :ol))
    (-> ctx
        (assoc :items (conj (:items ctx) (process-list-item ztx ctx)))
        (assoc :item line)
        (assoc :sub-items []))
    (-> (apply-transition ztx :end-list ctx nil)
        (assoc :state :none)
        (assoc :push-back true))))

(defmethod apply-transition :list-add-sub-elem
  [ztx _ ctx line]
  (-> ctx
      (assoc :sub-items (conj (:sub-items ctx) line))))

(defmethod apply-transition :end-list
  [ztx _ ctx line]
  (-> (update ctx :result conj (into [(:state ctx)]
                                     (mapv (fn [x] x)
                                           (conj (:items ctx)
                                                 (process-list-item ztx ctx)))))
      (assoc :state :none :items [] :sub-items [] :item nil)))

(defmethod apply-transition
  :default
  [ztx a ctx line]
  (println ::missed-transition a)
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

