(ns zd.db
  (:require [zd.datalog :as d]))

(defn has-children? [ztx dn]
  (let [q '{:find [(pull ?e [:xt/id])]
            :in [docname]
            :where [[?e :parent docname]]
            :limit 1}]
    (not-empty (d/query ztx q dn))))

(defn children-count [ztx dn]
  (d/query ztx
           '{:find [(count ?id)]
             :where [[?e :parent docname]
                     [?e :xt/id ?id]]
             :in [docname]}
           dn))

;; conversion of dn to str is required
(defn children [ztx dn & [page]]
  (let [query '{:find [?id ?lu]
                :where [[?e :parent docname]
                        [?e :xt/id ?id]
                        [?e :meta/last-updated ?lu]]
                :in [docname]
                #_:order-by #_[[?id :asc]]
                :order-by [[?lu :desc]]
                :limit 24
                :offset 0}
        query* (cond-> query
                 (some? page) (assoc :offset (* 24 (- (read-string page) 1))))]
    (d/query ztx query* dn)))
