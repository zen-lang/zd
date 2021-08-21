(ns zd.parser-test
  (:require [zd.parser :as sut]
            [matcho.core :as matcho]
            [clojure.test :as t]))

(t/deftest test-parser


  (matcho/match
    (sut/parse {} "

:zen/tags #{zd/doc}
:zen/authors [ gh.niquola ]

:zd/md>

## Welcome to zd

")

    {:zen/tags #{'zd/doc},
     :zen/authors ['gh.niquola],
     :zd/md> "\n## Welcome to zd\n"})




  )
