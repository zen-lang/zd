(ns zd.parse-test
  (:require
   [zen.core :as zen]
   [zd.parse :as sut]
   [matcho.core :as matcho]
   [clojure.java.io :as io]
   [clojure.test :as t]))

(def ztx (zen/new-context))
(def case-1 "
:title \"Title\"
:summary /
 This is a summary
 Which is multiline
 Realy

^test/format :json
:footer {:key \"val\"}

:comments:0
~:author \"auth\"
~:date \"date\"

:comments:1
~:author \"auth2\"
~:date \"date2\"
")


(t/deftest test-zd-parse

  (t/is (= (sut/parse ztx case-1)
           {:doc
            [{:path [:title], :annotations {}, :data "Title"}
             {:path [:summary],
              :annotations {'format :zentex},
              :data "This is a summary\n Which is multiline\n Realy"}
             {:path [:footer], :annotations {'format :json}, :data {:key "val"}}
             {:path [:comments 0], :annotations {}, :data nil}
             {:path [:comments 0 :author], :annotations {}, :data "auth"}
             {:path [:comments 0 :date], :annotations {}, :data "date"}
             {:path [:comments 1], :annotations {}, :data nil}
             {:path [:comments 1 :author], :annotations {}, :data "auth2"}
             {:path [:comments 1 :date], :annotations {}, :data "date2"}],
            :resource
            {:title "Title",
             :summary "This is a summary\n Which is multiline\n Realy",
             :footer {:key "val"},
             :comments [{:author "auth", :date "date"} {:author "auth2", :date "date2"}]}}))
  







  )
