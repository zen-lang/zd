(ns zd.parse-test
  (:require
   [zen.core :as zen]
   [zd.parse :as sut]
   [matcho.core :as matcho]
   [clojure.test :as t]))

(defmethod sut/annotation
  :test/format
  [_ param]
  {:content param})

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

^view-only
:view-only {:a 1}

:comments:1
~:author \"auth2\"
~:date \"date2\"
")


(t/deftest test-zd-parse

  (matcho/match
   (sut/parse ztx case-1)
   {:doc
    [{:path [:title], :data "Title"}
     {:path [:summary], :data "This is a summary\n Which is multiline\n Realy"}
     {:path [:footer], :data {:key "val"}, :annotations {:content :json}}
     {:path [:comments 0], :data nil}
     {:path [:comments 0 :author], :data "auth"}
     {:path [:comments 0 :date], :data "date"}
     {:path [:view-only], :data {:a 1}, :annotations {:view-only true}}
     {:path [:comments 1], :data nil}
     {:path [:comments 1 :author], :data "auth2"}
     {:path [:comments 1 :date], :data "date2"}],
    :resource
    {:title "Title",
     :summary "This is a summary\n Which is multiline\n Realy",
     :footer {:key "val"},
     :comments [{:author "auth", :date "date"} {:author "auth2", :date "date2"}],
     :view-only nil?}})







  )
