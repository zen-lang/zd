(ns zd.methods
  (:require
   [clojure.string :as str]
   [zen.core :as zen]
   [stylo.core :refer [c]]))

(defmulti render-key      (fn [ztx {pth :path}] pth))
(defmulti key-data        (fn [ztx pth data] pth))
(defmulti title-actions   (fn [ztx block] :override))
(defmulti render-block    (fn [ztx {{blk :block} :annotations}] (keyword blk)))
(defmulti render-content  (fn [ztx {{cnt :content} :annotations}] (keyword cnt)))
(defmulti annotation      (fn [nm params] (keyword nm)))
(defmulti inline-method   (fn [ztx m arg ctx] (keyword m)))
(defmulti inline-function (fn [ztx m arg ctx] (keyword m)))
(defmulti process-block   (fn [ztx tp args cnt] tp))

(defmulti rendercontent (fn [ztx ctx block]
                          (get-in block [:ann :zd/content-type])))

(defmethod rendercontent :default
  [ztx ctx block]
  [:pre (pr-str block)])

(defmulti renderkey (fn [ztx ctx block]
                      (or (get-in block [:ann :block])
                          (:key block))))

(defmethod renderkey :default [ztx ctx {kp :key :as block}]
  [:div.zd-block
   [:h2 {:class (str "zd-block-title " (name (c :flex :items-baseline)))}
    [:div {:class (c :flex :flex-1)}
     ;; TODO add collapse annotation for default view?
     [:a {:id kp :href (str "#" kp)} (str/capitalize (name kp))]]]
   [:div.zd-content (rendercontent ztx ctx block)]])

(defmulti layout
  (fn [ztx zd-config hiccup doc] (zen/engine-or-name zd-config)))

;; TODO add ctx
(defmulti widget (fn [ztx wgt page & [opts]] (keyword wgt)))

(defmethod widget
  :default
  ;; TODO maybe return error?
  [ztx wgt page & [opts]]
  [:div {:class (c [:text :red-500])}
   "Widget - " (pr-str wgt) " is not implemented"])

(defmulti eval-macro! (fn [ztx doc docpath [expr & args]] expr))

(defmethod eval-macro! :default
  [ztx doc docpath macro]
  {:error {:message (str (pr-str macro) " implementation not found")
           :type "macro-notfound"
           :docpath docpath}})
