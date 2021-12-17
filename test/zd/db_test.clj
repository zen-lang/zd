(ns zd.db-test
  (:require
   [zen.core :as zen]
   [zd.db :as sut]
   [zd.methods]
   [zd.impl]
   [matcho.core :as matcho]
   [clojure.java.io :as io]
   [clojure.test :refer :all]))

#_(remove-ns 'zd.db-test)

(defmacro find-block-and-match [ztx sym pth & [pattern]]
  `(let [blocks# (zd.db/get-doc ~ztx ~sym)
         block# (->> blocks#
                     (filter #(= ~pth (:path %)))
                     first)]
     (when ~pattern
       (matcho/match block# ~pattern))
     block#))

(defmethod zd.methods/annotation
  :tooltip
  [nm params]
  {:block :tooltip
   :type (or params "warn")})

(def ztx (zen/new-context))

(deftest test-zd-db

  (sut/load-dirs ztx #{"docs"})

  (:zdb @ztx)
  (:zrefs @ztx)

  (zen/read-ns ztx 'aidbox)

  (sut/load-content! ztx {:resource-path "team/vlad.zd"
                          :content "
:zen/tags #{aidbox/person}
:name \"Vlad Ganshin\"
:birth-date \"1994-09-26\"
"})

  (find-block-and-match
   ztx 'team.vlad
   [:zd/errors]
   nil?)


  (do
    (sut/load-content! ztx {:resource-path "team/noname.zd"
                            :content  "
:zen/tags #{aidbox/person}
:some-field 123
"})
    (sut/get-page ztx 'team.noname)
    )

  (find-block-and-match
   ztx 'team.noname
   [:zd/errors]
   {:data [{:message ":name is required"
            :type "require"
            :path [:name]}]})

  (zen.core/get-symbol ztx 'aidbox/person)

  (testing "tags inheritance test"

    (sut/load-content! ztx {:resource-path "project.zd"
                            :content "
:zd/child-tags #{aidbox/project}"})

    (sut/load-content! ztx {:resource-path "project/obscure.zd"
                            :content "
:zen/tags #{}
:period:start \"2019-09-16\"

"})




    (matcho/match
     (sut/get-resource ztx 'project.obscure)
     {:zen/tags #{'aidbox/project}})

    (sut/get-page ztx 'project.obscure)

    )


  (testing "annotations from zen/schema"

    (zen/load-ns ztx '{ns aidbox.test
                        testcase
                        {:zen/tags #{zen/schema zen/tag}
                         :type zen/map
                         :keys {:id {:type zen/string}
                                :date {:type zen/date :zd/annotations {:block :badge}}
                                :author {:type zen/string}
                                :prerequisites {:type zen/vector
                                                :every {:type zen/map
                                                        :keys {:desc {:type zen/string}
                                                               :tip {:type zen/string  :zd/annotations {:block :tooltip :type "warn"}}}}}
                                :steps {:type zen/vector
                                        :every {:type zen/map
                                                :keys {:desc {:type zen/string}
                                                       :tip {:type zen/string  :zd/annotations {:block :tooltip :type "warn"}}}}}
                                :expected-result {:type zen/string}}}})

    (sut/load-content! ztx {:resource-path "aidbox.test.create-new-box.zd"
                            :content "

:zen/tags #{aidbox.test/testcase}

:id \"create-new-box\"

^badge
:date \"2021-12-17\"

:steps:0
~:desc \"Open https://aidbox.app\"
~:tip \"Check your internet connection\"

:steps:1
~:desc \"Create new box\"
^tooltip \"info\"
~:tip \"Are you ready for some magic?\"

:expected-result \"box created\"

"})

    (find-block-and-match
     ztx 'aidbox.test.create-new-box [:date]
     {:annotations {:block :badge}})

    (find-block-and-match
     ztx 'aidbox.test.create-new-box [:steps 0 :tip]
     {:annotations {:block :tooltip :type "warn"}})

    (find-block-and-match
     ztx 'aidbox.test.create-new-box [:steps 1 :tip]
     {:annotations {:block :tooltip :type "info"}})

    (sut/get-page ztx 'readme)

    (find-block-and-match
     ztx 'readme [:loaded-yaml]
     {:data {:name "Name" :email "Email"}})

    (find-block-and-match
     ztx 'readme [:loaded-txt]
     {:data #"Simple text."})

    (find-block-and-match
     ztx 'readme [:not-loaded-file]
     {:data {:message "docs/ups.yaml (No such file or directory)"},
      :annotations {:block :error}})


    )





  )
