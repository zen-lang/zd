(ns zd.db
  (:require [zd.datalog :as d]
            [clojure.string :as str]))

(def search-clause '[(text-search :title search-text) [[?e]]])

(defn valid-search? [search-text]
  (and (string? search-text)
       (not (str/blank? search-text))))

(defn search [ztx search-text & [dn page]]
  (let [q '{:find [?id]
            :where [[?e :xt/id ?id]
                    #_[?e :meta/last-updated ?lu]]
            :in [#_docname]
            :order-by [[?id :asc]]
            ;; TODO bring back last updated?
            #_:order-by #_[[?lu :desc]]
            :offset 0}

        query (cond-> q
                (some? dn) (-> (update :where conj '[?e :parent docname])
                               (update :in conj 'docname))
                ;; TODO bring back pagination to search?
                (some? page) (assoc :offset (* 24 (- (read-string page) 1)))
                (valid-search? search-text) (-> (update :where conj search-clause)
                                                (update :in conj 'search-text)))

        args (cond-> [ztx query]
               (some? dn) (conj dn)
               ;; here we use fuzzy lucene trailing wildcard
               (valid-search? search-text) (conj (str (str/trim search-text) "*")))]
    (apply d/query args)))

(defn navbar-docs [ztx]
  (let [{r :root} (d/get-state ztx)]

    {:views
     (d/query ztx '{:find [?id]
                    :where [[?e :xt/id ?id]
                            [(clojure.string/includes? ?id "_views.")]]})
     :templates
     (d/query ztx '{:find [?id]
                    :where [[?e :xt/id ?id]
                            [(clojure.string/includes? ?id "_template")]]
                    :order-by [[?id :asc]]})
     :schemas
     (d/query ztx '{:find [?id]
                    :where [[?e :xt/id ?id]
                            [(clojure.string/includes? ?id "_schema")]]})

     :docs (d/query ztx
                    '{:find [?id ?mu (pull ?e [:section])]
                      :where [[?e :xt/id ?id]
                              [(some? ?mu)]
                              [?e :menu-order ?mu]]
                      :order-by [[?mu :asc]]})}))
