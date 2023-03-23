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
  (loader/hard-reload! ztx ["test/zd/tdocs"]))

(deftest document-tree-loaded
  (load! ztx)

  (->> ['customers 'customers.flame 'people.john 'people.todd]
       (map #(loader/get-doc ztx %))
       (every? (fn [{m :zd/meta :as doc}]
                 (is (map? m))
                 (is (not-empty m))
                 (is (seq (dissoc doc :zd/meta)))))))

(deftest macros-loaded
  (load! ztx)

  (matcho/assert
   {:macro-notfound
    {:error
     {:message string?
      :type "macro-notfound"}}
    :yaml-example {:key "myvalue", :another-key "another-value"}
    :string-file "just a string\n"
    :not-found
    {:error {:message string?
             :type "macro-eval"}}
    :office-locations map?
    :zd/meta
    {:ann
     {:macro-notfound {:zd/macro list?}
      :yaml-example {:zd/macro list?}
      :string-file {:zd/macro list?}
      :not-found {:zd/macro list?}
      :office-locations {:zd/macro list?}}}}

   (loader/get-doc ztx 'customers)))

(deftest referenced-loaded
  (load! ztx)

  (testing "edn links loaded"
    (:zrefs @ztx)
    (matcho/match
     '{rdfs.class {customers #{[:meta :tags :#]}}
       people.john {customers #{[:best-customer] [:desc]}
                    customers.flame #{[:ceo] [:founder]}}
       tags.data-platform {customers.flame #{[:tags :#]}}
       tags.telemed {customers.flame #{[:tags :#]}}}))

  (testing "zentext links and mentions loaded"
    (matcho/assert
     '{people.todd {customers #{[:desc]}}
       customers.flame {customers #{[:desc]}}
       people.john {customers #{[:best-customer] [:desc]}}}
     (:zrefs @ztx)))

  ;; TODO make links formats same?
  (testing "backlinks are collected"
    (matcho/assert {:zd/meta {:backlinks [{:to 'customers.flame :path [:desc] :doc 'customers}]}}
                   (loader/get-doc ztx 'customers.flame))

    (matcho/assert {:zd/meta {:backlinks [{:to 'people.john :path [:desc] :doc 'customers}
                                          {:to 'people.john :path [:best-customer] :doc 'customers}
                                          {:to 'people.john :path [:ceo] :doc 'customers.flame}
                                          {:to 'people.john :path [:founder] :doc 'customers.flame}]}}
                   (loader/get-doc ztx 'people.john))

    (matcho/assert {:zd/meta {:backlinks [{:to 'people.todd :doc 'customers :path [:desc]}]}}
                   (loader/get-doc ztx 'people.todd))))

(deftest block-meta-added
  (load! ztx)

  (matcho/assert
   {:zd/meta
    {:ann {:rel {:zd/content-type :edn, :badge {}},
           :tags {:zd/content-type :edn, :badge {}},
           :icon {:zd/content-type :edn, :none {}},
           :country {:zd/content-type :edn, :badge {}}}}}
   (loader/get-doc ztx 'customers.flame)))

(deftest subdocuments-loaded

  (load! ztx)

  (def doc (loader/get-doc ztx 'customers))

  (matcho/assert
   {:zd/subdocs
    {:nested
     {:tags set?
      :countries set?}}}
   doc)

  (def subdoc-ann
    (get-in doc [:zd/subdocs :nested :zd/meta :ann]))

  (is (contains? (:tags subdoc-ann) :badge))
  (is (contains? (:countries subdoc-ann) :badge)))
