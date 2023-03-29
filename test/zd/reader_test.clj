(ns zd.reader-test
  (:require
   [matcho.core :as matcho]
   [zen.core :as zen]
   [clojure.java.io :as io]
   [zd.reader :as reader]
   [clojure.test :refer :all]
   [clojure.string :as str]))

(deftest test-parser

  (def ztx (zen/new-context {}))

  (def sts (slurp (io/resource "zd/parser.zd")))

  (def result (reader/parse ztx {} sts))

  (is (empty? (get-in result [:zd/meta :errors])))

  (testing "inline keypaths are parsed with annotations"
    (matcho/assert
     {:title "my title"
      :myservice/api-endpoint "id=128"
      :desc "my desc"
      :myemptykey str/blank?
      :map-inline {:mykey {:myval 1}}
      :query '(load "file.json")
      :zd/meta {:ann {:desc {:badge {:text-size :large}}
                      :query {:table [:name :age]}}}}
     result))

  (defn strings? [coll]
    (every? string? coll))

  (testing "multiline keypaths are parsed with annotations"
    ;; TODO think about adding (read) multimethod for each content type
    (matcho/assert
     {:query-map {:where [] :limit 100}
      :another-query {:one-line :edn}
      :clients str/blank?
      :empty-edn str/blank?
      :nil-edn str/blank?
      :text "\nIt is truly multiline\n\n* and\n** has\n*** lists\n"
      :zd/meta {:ann
                {:query-map {:zd/content-type :edn}
                 :clients {:zd/content-type :datalog}
                 :empty-edn {:zd/content-type :edn}
                 :another-query {:zd/content-type :edn}
                 :nil-edn {:zd/content-type :edn}
                 :text {:zd/content-type :zentext}}}}

     result))

  (def keys-order
    [:title :myservice/api-endpoint :myemptykey :desc :map-inline :query :empty-edn :nil-edn
     :query-map :another-query :text :customers :clients :nested :nested-2])

  (testing "sub documents are parsed recursively"
    (matcho/assert
     {:zd/meta
      {:doc keys-order}
      :zd/subdocs
      {:nested
       {:zd/meta {:doc [:mykey :another-key :fish-key :path]}
        :zd/subdocs {:path {:zd/meta {:doc [:some-edn :mykey]}}}}
       :nested-2 {:zd/meta {:doc [:super-key]}}}}
     result)))

(deftest parsing-errors

  (def ztx (zen/new-context {}))

  (def err (slurp (io/resource "zd/errors.zd")))

  (is (map? (reader/parse ztx {} "just a string")))

  (def result (reader/parse ztx {} err))

  (is (map? result))

   ;; TODO fix :no-edn key parsing
  (matcho/assert {:zd/meta
                  {:errors
                   ;; TODO think about this block undefined error
                   [{:type :edn-reading-error,
                     :path [:map-inline],
                     :message "Invalid edn format"}
                    {:type :edn-reading-error,
                     :path [:wrong-multiline],
                     :message "Invalid edn format"}
                    {:type :edn-reading-error,
                     :path [:invalid-edn],
                     :message "Invalid edn format"}
                    {:type :block-undefined
                     :message "can not parse block"
                     :lines list?}]}
                  :invalid-multiline string?
                  :wrong-multiline string?
                  :map-inline string?
                  :invalid-edn  string?}
                 result))

(deftest datalog-query
  (def ztx (zen/new-context {}))

  (def sts (slurp (io/resource "zd/parser.zd")))

  (def result (reader/parse ztx {} sts))

  (matcho/assert
   {:customers '{:find [(pull e [:xt/id :rel :needs :tags])],
                :where [[e :parent "organizations"] [e :rel "rel.lead"] [e :title et]],
                :sort-by [[et :desc]]}
    :zd/meta {:ann {:customers {:zd/content-type :datalog
                                :table-of vector?}}}}
   result))
