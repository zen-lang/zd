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

(def ztx (zen/new-context {}))

(defn load! [ztx]
  ;; TODO add trailing slash validation in system config
  (loader/load-docs! ztx ["test/zd/tdocs"]))

(deftest document-tree-loaded
  (load! ztx)

  (matcho/assert
   {:zd/meta map?
    :title string?
    :desc string?}
   (loader/get-doc ztx 'customers))

  (matcho/assert
   {:zd/meta map?
    :title string?}
   (loader/get-doc ztx 'customers.flame)))

(deftest referenced-parsed
  (load! ztx)

  (testing "edn links loaded"
    (matcho/assert
     '{rdfs.class {customers #{[:meta :tags :#]}}
       people.john
       {customers #{[:best-customer] [:desc]}
        customers.flame #{[:ceo] [:founder]}}
       tags.dev-team {customers.flame #{[:tags :#]}}}
     (:zrefs @ztx)))

  (testing "zentext links and mentions loaded"
    (matcho/assert
     '{people.todd {customers #{[:desc]}}
       customers.flame {customers #{[:desc]}}
       people.john {customers #{[:best-customer] [:desc]}}}
     (:zrefs @ztx))))
