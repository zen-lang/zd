(ns zd.methods
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [zen.core :as zen]
   [stylo.core :refer [c]]))

;; renders content of a block with :zd/content-type annotation
(defmulti rendercontent (fn [ztx ctx block]
                          (get-in block [:ann :zd/content-type])))

;; by default just pretty prints the content
(defmethod rendercontent :default
  [ztx ctx {:keys [data] :as block}]
  [:pre (with-out-str (pprint/pprint data))])

;; renders key of a document with provided annotation
;; or by a block name
(defmulti renderkey (fn [ztx ctx block]
                      (if-let [[block-key _]
                               (->> (:ann block)
                                    (remove (fn [[k _]]
                                              (= "zd" (namespace k))))
                                    (first))]
                        block-key
                        (:key block))))

;; by default add a header and renders content of a block
(defmethod renderkey :default [ztx ctx {kp :key d :data :as block}]
  [:div {:class (c [:py 3])}
   [:div {:class (c :flex :flex-row :justify-between [:bg "white"])}
    [:div
     [:span {:class (c [:text :orange-500])} ":"]
     [:span {:class (c [:text :gray-600])} kp]]
    [:div {:class (c [:text :gray-500])}
     (get-in block [:ann :zd/content-type])]]
   (when-not (and (string? d) (str/blank? d))
     [:div {:class (c [:bg "#F7FAFC"])}
      (rendercontent ztx ctx block)])])

;; zentext methods
(defmulti inline-method   (fn [ztx m arg ctx] (keyword m)))
(defmulti inline-function (fn [ztx m arg ctx] (keyword m)))
(defmulti process-block   (fn [ztx tp args cnt] tp))

;; renders an application layout (index.html, navigation, loads js)
;; required.
(defmulti layout
  (fn [ztx zd-config hiccup doc] (zen/engine-or-name zd-config)))

;; renders UI widget that is updated asynchronously
;; TODO add ctx

(defmulti widget (fn [ztx ctx page & [opts]] (keyword (:widget ctx))))

(defmethod widget
  :default
  ;; TODO maybe return error?
  [ztx wgt page & [opts]]
  [:div {:class (c [:text :red-500])}
   "Widget - " (pr-str wgt) " is not implemented"])

;; evaluates a macro during the document loading phase
(defmulti eval-macro! (fn [ztx doc docpath [expr & args]] expr))

(defmethod eval-macro! :default
  [ztx doc docpath macro]
  {:error {:message (str (pr-str macro) " implementation not found")
           :type "macro-notfound"
           :docpath docpath}})

;; renders cell of a UI table component
(defmulti render-cell (fn [ztx ctx key data] key))

(defmethod render-cell :default
  [ztx ctx k row]
  (when-let [v (get row k)]
    (rendercontent ztx ctx {:data v :key k :ann {:zd/content-type :edn}})))
