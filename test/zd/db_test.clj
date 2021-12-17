(ns zd.db-test
  (:require
   [zen.core :as zen]
   [zd.db :as sut]
   [matcho.core :as matcho]
   [clojure.java.io :as io]
   [clojure.test :refer :all]))

#_(remove-ns 'zd.db-test)

(def ztx (zen/new-context))

(deftest test-zd-db

  (sut/load-dirs ztx #{"docs"})

  (:zdb @ztx)
  (:zrefs @ztx)

  (zen/read-ns ztx 'aidbox)

  (sut/load-content! ztx "team/vlad.zd" "
:zen/tags #{aidbox/person}
:name \"Vlad Ganshin\"
:birth-date \"1994-09-26\"
")

  (is (empty? (:errors (sut/get-page ztx 'team.vlad))))

  (sut/load-content! ztx "team/noname.zd" "
:zen/tags #{aidbox/person}
")

  (is (first (:errors (sut/get-page ztx 'team.noname))))

  (matcho/match
   (sut/get-page ztx 'team.noname)
   {:errors [{:message ":name is required"
              :type "require"
              :path [:name]}
             nil?]})

  (sut/get-page ztx 'team.noname)

  (testing "tags inheritance test"

    (sut/load-content! ztx "project.zd" "
:child-tags #{aidbox/project}")

    (sut/load-content! ztx "project/obscure.zd" "
:zen/tags #{}
:period:start \"2019-09-16\"

")




    (matcho/match
     (sut/get-resource ztx 'project.obscure)
     {:zen/tags #{'aidbox/project}})

    (sut/get-page ztx 'project.obscure)

    )





  )
