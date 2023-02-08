(ns zd.parser-test
  (:require
   [zd.parser :as sut]
   [clojure.test :refer :all]))

(def example
"
^badge {:param 100}
:title \"value\"
:logo \"logo\"
:text /

Multiline text

:edn-value
{:where []
 :limit 100}

&nested
:part_of .
:title \"Sub\"

")

(def result
  {:zd/name 'resource
   :zd/order [:title :logo :text :edn-value 'nested]
   :zd/keys {:title {:value "original value"
                     :annotations {:badge {:param 100 :text "..."}}}}
   :zd/contained {'sub-resource {:zd/name 'resource.sub-resource}}
   :title "title"
   :value "value"
   :text "\nMultiline text\n"})

(def ztx (atom {}))

;; (defn print-zd [ztx res]
;;   (let [all-keys  (into #{} (concat (->> (keys res) (remove #(= (namespace %) "zd"))) (keys :zd/contained)))
;;         ordered-keys (:zd/order res)
;;         extra-keys (clojure.set/difference (into #{} ordered-keys) all-keys)
;;         keys-to-print (into ordered-keys (->> extra-keys (sort-by str)))]))

;; (defn update-key [res key val & [opts]])


(deftest test-parser

  (sut/parse ztx {} ":title \"value\"")



  )
