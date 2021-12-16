(ns zd.zentext-test
  (:require [zd.zentext :as sut]
            [matcho.core :as matcho]
            [clojure.test :as t]))

#_(remove-ns 'zd.zentext-test)

(def ztx (zen.core/new-context))

(defmacro match [md & [pattern]]
  `(let [res# (sut/parse-block ztx ~md)]
     (matcho/match res# ~pattern)
     res#))

(defmacro match-inline [md & [pattern]]
  `(let [res# (sut/parse-inline ztx ~md)]
     (matcho/match res# ~pattern)
     res#))




(def paragraphs
  ["
This is first paragraph

This is another paragrahp
"
   [:div
    [:p "This is first paragrahp"]
    [:p "This is another paragrahp"]]])


(def ref
  ["This is first paragraph  #symbol.to.ref:key:path and other case" 
   [:p "This is first paragraph " [:a {:href "/symbol.to.re"} "symbol.to.ref"]]])

(def inline-method
  ["This is first paragraph  [[ref symbol.to.ref:key:path This is title]] and [[img data.png title]] and [[unexisting ups]]"
   [:p
    "This is first paragraph "
    [:a {:href "/symbol.to.re"} "This is title" ]
    " and "
    [:img {:src "data.png" :alt "title"}]
    " and "
    [:span {:class "error"} "Method 'unexisting not defined"]]])

;; (defmethod sut/function
;;   'test/current-date
;;   [data])

(def inline-functions
  ["This is first paragraph  ((test/current-date)) and ((test/search {:tag #{incident}}))"
   [:p
    "This is first paragraph "
    [:div "CURRENT_DATE"]
    " and "
    [:div "TEST SEARCH: {:tag }"]]])

(def block-elements
  [
" This code sample [[html <img src=\"\">]]
```code clojure
  (range 100)
```

```html
<img src=\"\">
```

"
   [:div [:p "This code sample"]
    [:code [:pre "  (range 100)"]]]])

(def lists
  [
   "
This is text multiline
((function arg))

Pargraph
```code
```
* list 1 multi line
* list 2
..* sub item 1
..* sub item 2
* list 3

* another
* list

1) item 1
1) item 2
..1) sub item
..2) another

* item
..1) subitem
..2) subitem

1) item
..* subitem
..* subitem

"
   [:div
    [:p "..."]
    [:ul
     [:li "List 1"]
     [:li "List 2"
      [:ul
       [:li "sub 1"]
       [:li "sub 2"]]]

     [:ul
      [:li "another"]
      [:li "list"]]]]])

;; text -> split by lines
;; collect blocks

;; state(block,list,paragraph)


;; block - eat all lines till the end - call multimethod
;; paragraphs till new line or block (```) start or list start (*, '\d+\)')

;; list - until new line or block other list
;;   list-item+level until same list marker  - remove '... - apply list algorythm



(def sample
  "
This is a paragraph with #aidbox.customers
[[img logo.png]]
line2

line2.1
line2.2
```code yaml
name:
- given: [Vl]
```
* 1
..* 1-1
..* 1-2
....* 1-2-1
..* 1-3
* 2

final paragraph [[src box/zrc/aidbox.edn#config]] "


  )

(t/deftest test-parser

  (match "paragraph" [:div [:p "paragraph"]])

  (match-inline
   "This is a #link.to-entity and another #link.to"
   ["This is a "
    [:a {:href "/link.to-entity"} "link.to-entity"]
    " and another "
    [:a {:href "/link.to"} "link.to"]])

  (match
   "```code sql
select 1
```
"
   [:div [:pre [:code {} "select 1"]]])

  (match "
* list 1
* list 2
"
         [:div [:ul [:li "list 1"] [:li "list 2"]]])


  (match
   "
* list
..* list
"
   [:div [:ul [:li "list" [:ul [:li "list"]]]]])

  (match "
* 1
* 2
..* 2-1
....* 2-1-1
..* 2-2
* 3
"
         [:div
          [:ul
           [:li "1"]
           [:li "2"
            [:ul
             [:li
              "2-1"
              [:ul
               [:li "2-1-1"]]]
             [:li "2-2"]]]
           [:li "3"]
           nil?]])


  (match
   "* #link.to"
   [:div [:ul [:li [:a {:href "/link.to"} "link.to"]]]])


  (match
   "
1) ordered list 1
2) ordered list 2
"
   [:div
    [:ol
     [:li "ordered list 1"]
     [:li "ordered list 2"]]]
   )

  (match
   "
1) ordered list 1
1) ordered list 2
..* 2-1
"
   [:div
    [:ol
     [:li "ordered list 1"]
     [:li "ordered list 2"
      [:ul [:li "2-1"]]]]])

  (match
   "
* ordered list 1
..1) 1-1
..1) 1-2
"
   [:div
    [:ul
     [:li "ordered list 1"
      [:ol
       [:li "1-1"]
       [:li "1-2"]]]]])

  (clojure.pprint/pprint
   (sut/parse-block ztx sample))








  )
