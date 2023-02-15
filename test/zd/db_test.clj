(ns zd.db-test
  (:require
   [clojure.string :as str]
   [zen.core :as zen]
   [zd.db :as db]
   [zd.methods]
   [zd.impl]
   [matcho.core :as matcho]
   [clojure.java.io :as io]
   [clojure.test :refer :all]))

(declare ztx)

(defn prepare! []
  (def ztx (zen/new-context)))

(deftest test-zd-db
  (prepare!)

  (def zendoc
    "^badge\n:tags #{person}\n:name \"Vlad Ganshin\"\n:birth-date \"1994-09-26\"\n:goals:short-term / ")

  (db/load-content! ztx {:resource-path "team/vlad.zd" :content zendoc})

  (is (empty? (zen/errors ztx)))

  (matcho/match (db/get-page ztx 'team.vlad)
    {:doc
     [{:path [:tags] :data #{'person} :annotations {:block :badge}}
      {:path [:name] :data "Vlad Ganshin" :annotations {}}
      {:path [:birth-date] :data "1994-09-26" :annotations {}}
      {:path [:goals :short-term] :data str/blank? :annotations {}}]
     :resource {:tags #{'person}
                :name "Vlad Ganshin"
                :birth-date "1994-09-26"
                :goals {:short-term str/blank?}}
     :zd/name 'team.vlad
     :zd/file "team/vlad.zd"
     :zd/path nil?
     :name 'team.vlad}))
