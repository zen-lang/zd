(ns zd.db-test
  (:require
   [zen.core :as zen]
   [zd.db :as sut]
   [matcho.core :as matcho]
   [clojure.java.io :as io]
   [clojure.test :as t]))

(def ztx (zen/new-context))

(t/deftest test-zd-db

  (sut/load-dirs ztx #{"docs"})

  (:zdb @ztx)




  )
