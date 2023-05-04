(ns zd.db
  (:require [zd.datalog :as d]
            [clojure.string :as str]))

(def search-clause '(or [(text-search :meta/docname search-text) [[?e]]]
                        [(text-search :title search-text) [[?e]]]))

(defn valid-search? [search-text]
  (and (string? search-text)
       (not (str/blank? search-text))))

(defn children-count [ztx dn & [search-text]]
  (let [q '{:find [(count ?id)]
            :where [[?e :parent docname]
                    [?e :xt/id ?id]]
            :in [docname]}

        query
        (cond-> q
          (valid-search? search-text)
          (-> (update :where conj search-clause)
              (update :in conj 'search-text)))

        args (cond-> [ztx query dn]
               (valid-search? search-text) (conj search-text))]

    (apply d/query args)))

;; conversion of dn to str is required
(defn children [ztx dn & [page search-text]]
  (let [q '{:find [?id]
            :where [[?e :parent docname]
                    [?e :xt/id ?id]
                    #_[?e :meta/last-updated ?lu]]
            :in [docname]
            :order-by [[?id :asc]]
            #_:order-by #_[[?lu :desc]]
            :limit 24
            :offset 0}

        query (cond-> q
                (some? page) (assoc :offset (* 24 (- (read-string page) 1)))
                (valid-search? search-text) (-> (update :where conj search-clause)
                                                (update :in conj 'search-text)))

        args (cond-> [ztx query dn]
               (valid-search? search-text) (conj search-text))]

    (apply d/query args)))
