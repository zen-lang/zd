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
      (str/starts-with? l "* ") :list
      (re-matches #"^\d+\) " l) :olist
      (str/blank? l) :blank
      :else :p)))

(defn action [state line-type]
  (cond
    (and (= :none state) (= :blank line-type)) :none
    (and (= :none state) (= :p line-type)) :p
    (and (= :p state)    (= :p line-type)) :p
    (and (= :p state)    (= :blank line-type)) :none
    (and (= state :block) (not (= :block line-type))) :block
    (and (= state :block) (= :block line-type)) :none
    (and (not (= state :block)) (= line-type :block)) :block
    (and (= line-type :list)) :list
    (and (= state :list) (contains? #{:block :blank} line-type)) :none
    :else :tbd))

(defn update-context [ctx old-state new-state l]
  (cond
    (= [:none :p] [old-state new-state])
    (assoc ctx :lines [l] :state :p)

    (= [:p :p] [old-state new-state])
    (update ctx :lines (fn [x] (conj (or x []) l)))

    (= [:p :none] [old-state new-state])
    (-> ctx
        (update :res conj (into [:p] (:lines ctx)))
        (assoc :status :none :lines []))
    :else
    (assoc ctx :state new-state)))

(defn parse-block [ztx s]
  (let [lines (get-lines s)
        ctx {}
        res (loop [ctx  {:state :none :res [:div ]}
                   [l & ls] lines]
              (let [line-type (line-type l)
                    new-state (action (:state ctx) line-type)
                    new-ctx (update-context ctx (:state ctx) new-state l)]
                (println :* new-ctx)
                (if l
                  (recur new-ctx ls)
                  (:res ctx))))]
    (println res)))


;; (def rules
;;   {:block {"```"       {:ends "```"
;;                         :parser :block}
;;            "* "        {:parser :ul}
;;            "1. "       {:parser :ol}
;;            "# "        {:parser :h1     :line true}
;;            "## "       {:parser :h2     :line true}
;;            "### "      {:parser :h3     :line true}
;;            "#### "     {:parser :h4     :line true}
;;            "> "        {:parser :quote}}
;;    :block-regexp      {#"^\d+\. "   {:parser :ol     :line true}}

;;    :bindings {'echo (fn [& args] [:code (pr-str args)])
;;               'img  (fn [pth] [:img {:src pth}])
;;               '?    (fn [& args] (into [:ul] (->> (:bindings rules)
;;                                                  (mapv (fn [[k _]] [:li (str k)])))))}

;;    :tokens {"@"        {:parser :mention}
;;             "#"        {:parser :issue}
;;             "[-]"      {:parser :unchecked}
;;             "[x]"      {:parser :checked}
;;             "https://" {:parser :link}
;;             "http://"  {:parser :link}}

;;    :inline {"[["        {:parser :zen}
;;             "(("        {:parser :fn}
;;             "**"        {:parser :bold}
;;             "`"         {:parser :code}}})



;; (defmulti parse-token (fn [ztx cfg token] (:parser cfg)))

;; (defmethod parse-token :mention
;;   [ztx cfg token]
;;   (let [sym (subs token 1)]
;;     [:a {:href (str "/" sym)} sym]))

;; (defmethod parse-token :checked
;;   [ztx cfg token]
;;   [:input {:type "checkbox" :checked true}])

;; (defmethod parse-token :unchecked
;;   [ztx cfg token]
;;   [:input {:type "checkbox"}])

;; (defmethod parse-token :link
;;   [ztx cfg token]
;;   [:a {:href token} token])

;; (defmethod parse-token :issue
;;   [ztx cfg token]
;;   (when (re-matches #"^#\d+$" token)
;;     (let [sym (subs token 1)]
;;       [:a {:href (str "https://github.com/HealthSamurai/sansara/issues/" sym)} token])))


;; (defmethod parse-token :default
;;   [ztx cfg token]
;;   [:span [:pre (pr-str cfg) token]])

;; (defmulti render-inline (fn [ztx cfg token sc] (:parser cfg)))

;; (defn find-tail [token sc regexp]
;;   (if-let [m (re-matches regexp token)]
;;     m
;;     (when-let [m (.findInLine sc regexp)]
;;       (str token m))))

;; (defmethod render-inline
;;   :fn
;;   [ztx cfg token sc]
;;   (when-let [m (find-tail token sc #"[^)]*\)\)")]
;;     (let [expr (str/replace m #"(^\(|\)$)" "")
;;           res (try (sci.core/eval-string  expr {:bindings (:bindings rules)})
;;                    (catch Exception e
;;                      [:div {:class (c [:text :red-500])}
;;                       [:pre expr]
;;                       [:div (str "Error:" (.getMessage e))]]))]
;;       res)))

;; (defmulti render-zen (fn [x & args] x))

;; (defmethod render-zen "img"
;;   [x src & args]
;;   [:img {:src src}])

;; (defmethod render-zen "href"
;;   [x title & [href]]
;;   (if href
;;     [:a {:href href} title]
;;     [:a {:href title} title]))

;; (defmethod render-zen "src"
;;   [x src]
;;   [:a {:href (str "https://github.com/HealthSamurai/sansara/blob/master/" src)} src])


;; (defmethod render-zen :default
;;   [x & args]
;;   [:span [:b "Unknown render: " x] [:code (pr-str args)]])

;; (defmethod render-inline
;;   :zen
;;   [ztx cfg token sc]
;;   (when-let [m (find-tail token sc #"[^]]*\]\]")]
;;     (let [parts (-> m
;;                     (str/replace #"(^\[\[|\]\]$)" "")
;;                     (str/split #"\s+"))]
;;       (apply render-zen parts))))


;; (defmethod render-inline
;;   :bold
;;   [ztx cfg token sc]
;;   (when-let [m (find-tail (subs token 2) sc #"[^*]*\*\*")]
;;     [:b (str/replace m #"\*\*" "")]))

;; (defmethod render-inline
;;   :code
;;   [ztx cfg token sc]
;;   (when-let [m (find-tail (subs token 1) sc #"[^`]*`")]
;;     [:code (str/replace m #"`" "")]))

;; (defmethod render-inline
;;   :default
;;   [ztx cfg token sc]
;;   (println :NOT-IMPLEMENTED cfg)
;;   nil)

;; (defn parse-inline [ztx l]
;;   (let [sc (java.util.Scanner. l)]
;;     (loop [res []]
;;       (if (.hasNext sc)
;;         (let [token (.next sc)]
;;           (if-let [item (->> (:tokens rules)
;;                              (map (fn [[k p]]
;;                                     (when (str/starts-with? token k)
;;                                       (parse-token ztx p token))))
;;                              (filter identity)
;;                              (first))]
;;             (recur (conj res " " item))
;;             (if-let [item (->> (:inline rules)
;;                                (map (fn [[k p]]
;;                                       (when (str/starts-with? token k)
;;                                         (render-inline ztx p token sc))))
;;                                (filter identity)
;;                                (first))]
;;               (recur (conj res " " item))
;;               (recur (conj res " " token)))))
;;         res))))

;; (defmulti parse-block (fn [ztx block acc] (:parser block)))

;; (defmethod parse-block :p
;;   [ztx block  acc]
;;   (->> acc
;;        (mapcat (fn [l] (parse-inline ztx l)))
;;        (into [:p])))

;; (defmulti render-block (fn [ztx parser opts acc] parser))


;; (defmethod render-block
;;   :pre
;;   [ztx parser opts acc]
;;   [:pre
;;    (str/join "\n" acc)])

;; (defmethod render-block
;;   :code
;;   [ztx parser [lang & _] acc]
;;   [:pre [:code {:class (str "language-" lang " hljs")} (str/join "\n" acc)]])

;; (defmethod render-block
;;   :csv
;;   [ztx parser opts acc]
;;   (let [[headers & rows] (->> acc (mapv (fn [l] (into [] (str/split l #"\s*,\s*")))))]
;;     [:table
;;      [:thead
;;       (into [:tr] (mapv (fn [x] [:th x]) headers))]
;;      (->> rows
;;           (mapv (fn [r] (into [:tr] (mapv (fn [c] [:td c]) r))))
;;           (into [:tbody]))]))

;; (defmethod render-block
;;   :hiccup
;;   [ztx parser opts acc]
;;   (let [hic (edamame.core/parse-string (str/join "\n" acc))]
;;     hic))

;; (defmethod render-block :default
;;   [ztx parser opts acc]
;;   [:div
;;    [:b parser "?"]
;;    [:pre
;;     (str/join "\n" acc)]])

;; (defmethod parse-block :block
;;   [ztx block acc]
;;   (let [parts (-> (or (second (str/split (first acc) #"```")) "pre")
;;                   (str/split #"\s+"))
;;         proc (keyword (str/trim (first parts)))]
;;     (render-block ztx proc (rest parts) (rest acc))))

;; (defmethod parse-block :h1
;;   [ztx block acc]
;;   [:h1 (str/trim (second (str/split (first acc) #"\#+" )))])

;; (defmethod parse-block :h2
;;   [ztx block acc]
;;   [:h2 (str/trim (second (str/split (first acc) #"\#+" )))])

;; (defmethod parse-block :h3
;;   [ztx block acc]
;;   [:h3 (str/trim (second (str/split (first acc) #"\#+" )))])

;; (defmethod parse-block :h4
;;   [ztx block acc]
;;   [:h4 (str/trim (second (str/split (first acc) #"\#+" )))])

;; (defmethod parse-block :ul
;;   [ztx block acc]
;;   (loop [[l & ls] acc
;;          res [:ul]]
;;     (if (nil? l)
;;       res
;;       (recur ls (conj res (into [:li] (parse-inline ztx (str/replace l #"\s*\*\s*" ""))))))))

;; (defmethod parse-block :ol
;;   [ztx block acc]
;;   (loop [[l & ls] acc
;;          res [:ol]]
;;     (if (nil? l)
;;       res
;;       (recur ls (conj res (into [:li] (parse-inline ztx (str/replace l #"^\s*1\.\s*" ""))))))))

;; (defmethod parse-block :quote
;;   [ztx block acc]
;;   (loop [[l & ls] acc
;;          res [:blockquote]]
;;     (if (nil? l)
;;       res
;;       (recur ls (conj res (str (str/replace l #"\s*>\s*" "") "\n"))))))

;; (defmethod parse-block :default
;;   [ztx block acc]
;;   [:div
;;    [:b "Missed parser " (:parser block)]
;;    [:pre (pr-str block)]
;;    [:pre (str/join "\n" acc)]])


;; (defn detect-block [ztx rules l]
;;   (let [tl (str/trim l)]
;;     (if-let [rule (->> (:block rules)
;;                        (sort-by first)
;;                        (filter (fn [[k _]]
;;                                  (str/starts-with? tl k)))
;;                        (first))]
;;       (second rule)
;;       (if-let [rule (->> (:block-regexp rules)
;;                          (sort-by #(str (first %)))
;;                          (filter (fn [[k _]]
;;                                    (re-matches k tl)))
;;                          (first))]
;;         (second rule)
;;         (when-not (= "" tl)
;;           {:parser :p})))))

;; (defn end-of-block?
;;   [block l acc]
;;   (let [tl (str/trim l)]
;;     (if-let [end (:ends block)]
;;       (when (str/starts-with? l end)
;;         acc)
;;       (when (= tl "")
;;         acc))))


;; (defn parse [ztx s & [opts]]
;;   (let [sc (java.util.Scanner. s)]
;;     (loop [res [:div.markdown-body]
;;            block nil
;;            acc []]
;;       (if (.hasNextLine sc)
;;         (let [l (.nextLine sc)]
;;           (if block
;;             (if-let [acc (end-of-block? block l acc)]
;;               (recur (conj res (parse-block ztx block acc)) nil [])
;;               (recur res block (conj acc l)))
;;             (if-let [block (detect-block ztx rules l)]
;;               (if (:line block)
;;                 (recur (conj res (parse-block ztx block [l])) nil [])
;;                 (recur res block [l]))
;;               (recur res block acc))))
;;         (if block
;;           (conj res (parse-block ztx block acc))
;;           res)))))

;; (comment

;;   (def example
;;     "

;; # title

;; this is just a paragraph

;; * item 1
;; * item 2


;; ```clojure

;; (fn [x ] z)
;; ```


;; Just paragrahp
;; of text with [[this.is.a.link]]


;; > this is a quote
;; > multiline


;; just paragrahp

;; "
;;     )

;;   (def ztx  (atom {}))

;;   (println (pr-str (parse ztx example)))

;;   (def iexample "
;; this is a **paragraph** [[link]] @aidbox.team.niquola https://github.com    #78 and father   ((echo :key 12)) ((+ 10 20))
;; this is quite **bold**
;; this is **bold with spaces **
;; this is code `(fn :a)` and 
;; this is `@my.ns`
;; this is cb [-] and [x]
;; ")

;;   (println (pr-str (parse-inline ztx iexample)))



;;   )
