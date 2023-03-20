(ns zd.parser-test
  (:require
   [matcho.core :as matcho]
   [zen.core :as zen]
   [clojure.java.io :as io]
   [zd.parser :as parser]
   [clojure.test :refer :all]))

(deftest test-parser

  (def ztx (zen/new-context {}))

  (def sts (slurp (io/resource "zd/parser.zd")))

  (def result (parser/parse ztx {} sts))

  (testing "inline keypaths are parsed with annotations"
    (matcho/assert
     {:title "my title"
      :desc "my desc"
      :map-inline map?
      :query list?
      :zd/meta {:ann {:desc {:badge map?}
                      :query {:table [:name :age]}}}}
     result))

  (defn strings? [coll]
    (every? string? coll))

  (testing "multiline keypaths are parsed with annotations"
    ;; TODO think about adding (read) multimethod for each content type
    (matcho/assert
     {:query-map map?
      :text string?
      :zd/meta {:ann
                {:query-map {:zd/content-type :edn}
                 :text {:zd/content-type :zentext}}}}

     result))

  (testing "sub documents are parsed recursively"
    (matcho/assert
     {:zd/meta
      {:doc [:title :desc :map-inline :query :query-map :text :customers :nested :nested-2]}
      :zd/subdocs
      {:nested
       {:zd/meta {:doc [:mykey :another-key :mykey :path]}
        :zd/subdocs {:path {:zd/meta {:doc [:some-edn :mykey]}}}}
       :nested-2 {:zd/meta {:doc [:super-key]}}}}
     result)))

(deftest parsing-errors

  (def ztx (zen/new-context {}))

  (def sts (slurp (io/resource "zd/errors.zd")))

  (parser/parse ztx {} "just a string")

  (def result (parser/parse ztx {} sts))

  (is (map? result))

  (matcho/assert {:zd/errors [{:type :block-err}]} result))

(deftest datalog-query
  (def ztx (zen/new-context {}))

  (def sts (slurp (io/resource "zd/parser.zd")))

  (matcho/assert
   {:customers map?
    :zd/meta {:ann {:customers {:zd/content-type :datalog
                                :table-of vector?}}}}
   (parser/parse ztx {} sts)))
