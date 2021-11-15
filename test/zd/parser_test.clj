(ns zd.parser-test
  (:require [zd.parser :as sut]
            [matcho.core :as matcho]
            [clojure.java.io :as io]
            [clojure.test :as t]))

(def sample (slurp (io/resource "zd/parser_sample.zd")))
sample

(t/deftest test-parser


  (matcho/match
   (sut/parse {} "
:key 1
:nested~:key 2
:nested~:another 5
")
   {:zd/keys [[:key] [:nested :key] [:nested :another]],
    :key 1,
    :nested {:key 2, :another 5}})

  (matcho/match
   (sut/parse {} "
:tkey 1
:nested~
~:key 2
~:another 5
")

   {:zd/keys [[:tkey] [:nested] [:nested :key] [:nested :another]],
    :tkey 1,
    :nested {:key 2, :another 5}})

   (matcho/match
    (sut/parse {} "
:tkey 1
:nested~#~
~:key 1
~:another 2
:nested~#~
~:key 2
~:another 5
")

    {:zd/keys
     [[:tkey]
      [:nested 0]
      [:nested 0 :key]
      [:nested 0 :another]
      [:nested 1]
      [:nested 1 :key]
      [:nested 1 :another]],
     :tkey 1,
     :nested [{:key 1, :another 2} {:key 2, :another 5}]}
   )

  (matcho/match
   (sut/parse {} "
:content md/

Here is a content
multiline

:another-key 1
")

   {:zd/keys [[:content] [:another-key]],
    :content {:format "md", :content "\nHere is a content\nmultiline\n"},
    :another-key 1}
   
   )

  ;; (matcho/match (sut/parse {} sample)
  ;;               {:zd/book {:intro {:title "???"}}})




  )
