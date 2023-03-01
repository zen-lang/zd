(ns zd.loader-test
  (:require
   [zd.db :as db]
   [matcho.core :as matcho]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [zd.loader :as loader]
   [zen.core :as zen]
   [zd.core]
   [zen-web.core :as web]))

(ns-unmap *ns* 'ztx)

(defonce ztx (zen/new-context {}))

(defn load! [ztx]
  ;; TODO add trailing slash validation in system config
  (loader/load-docs! ztx ["test/zd/tdocs"]))

(load! ztx)

(deftest document-tree-loaded

  (matcho/assert
   {:zd/meta map?
    :title string?
    :desc #(every? string? %)}
   (loader/get-doc ztx 'customers))

  (matcho/assert
   {:zd/meta map?
    :title string?}
   (loader/get-doc ztx 'customers.flame)))

(deftest referenced-parsed
  (matcho/assert
   '{rdfs.class {customers #{[:meta :tags :#]}}
     people.john
     {customers #{[:best-customer]}
      customers.flame #{[:ceo] [:founder]}}
     tags.dev-team {customers.flame #{[:tags :#]}}}
   (:zrefs @ztx)))
