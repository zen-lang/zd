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
(defmethod renderkey :default [ztx ctx {kp :key d :data anns :ann :as block}]
  (let [render-inline?
        (and (not (:zd/multiline anns))
             (= (:zd/content-type anns) :edn)
             (not (map? d)))]
    [:div {:class (c [:py 4] :flex)
           :style (if render-inline?
                    {:flex-direction "row"}
                    {:flex-direction "column"})}
     [:div {:class (c :flex :flex-row :items-center :justify-between)}
      [:a {:id kp}
       [:span {:class (c [:text :orange-500])} ":"]
       [:span {:class (c [:text :gray-600] :text-sm {:text-transform "uppercase"})} kp]]
      #_[:div {:class (c [:text :gray-500] :text-sm)}
         (name (get-in block [:ann :zd/content-type]))]]
     (when-not (and (string? d) (str/blank? d))
       [:div {:style (when render-inline?
                       {:padding-left "1rem"})}
        (rendercontent ztx ctx block)])]))

;; zentext methods
(defmulti inline-method   (fn [ztx m arg ctx] (keyword m)))
(defmulti inline-function (fn [ztx m arg ctx] (keyword m)))
(defmulti process-block   (fn [ztx tp args cnt] tp))

;; renders UI widget that is updated asynchronously
(defmulti widget (fn [ztx ctx page & [opts]] (keyword (:widget ctx))))

(defmethod widget
  :default
  ;; TODO add ctx to args
  [ztx wgt page & [opts]]
  [:div {:class (c [:text :red-500])}
   ;; draw error status bar
   "Widget - " (pr-str wgt) " is not implemented"])

;; evaluates a macro during the document loading phase
(defmulti eval-macro! (fn [ztx doc docpath [expr & args]] expr))

(defmethod eval-macro! :default
  [ztx doc docpath macro]
  {:error {:message (str (pr-str macro) " implementation not found")
           :type "macro-notfound"
           :docpath docpath}})

(comment
  (ns-unmap *ns* 'renderkey)

  )
