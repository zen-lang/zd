(ns zd.zentext-test
  (:require
   [zd.zentext :as zentext]
   [zen.core :as zen]
   [matcho.core :as matcho]
   [clojure.test :as t]))

(defonce ztx (zen.core/new-context))

(defmacro match [md & [pattern]]
  `(let [res# (zentext/parse-block ztx ~md {})]
     (matcho/match res# ~pattern)
     res#))

(defmacro match-inline [md & [pattern]]
  `(let [res# (zentext/parse-inline ztx ~md {})]
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
    [:a {:href "/symbol.to.re"} "This is title"]
    " and "
    [:img {:src "data.png" :alt "title"}]
    " and "
    [:span {:class "error"} "Method 'unexisting not defined"]]])

(def inline-functions
  ["This is first paragraph  ((test/current-date)) and ((test/search {:tag #{incident}}))"
   [:p
    "This is first paragraph "
    [:div "CURRENT_DATE"]
    " and "
    [:div "TEST SEARCH: {:tag }"]]])

(def block-elements
  [" This code sample [[html <img src=\"\">]]
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
  ["
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

final paragraph [[src box/zrc/aidbox.edn#config]] ")

(t/deftest test-parser

  (match "paragraph" [:div [:p "\n" "paragraph"]])

  (match
   "```code sql
select 1
```
"
    [:div
     [:div.code-block
      [:pre {}
       [:code {} "select 1"]]]])

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
   "
1) ordered list 1
2) ordered list 2
"
    [:div
     [:ol
      [:li "ordered list 1"]
      [:li "ordered list 2"]]])

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

  (match
   "
1) ol 1-1
1) ol 1-2
* ul 1-1
* ul 1-2
"
    [:div
     [:ol
      [:li "ol 1-1"]
      [:li "ol 1-2"]]
     [:ul
      [:li "ul 1-1"]
      [:li "ul 1-2"]]])

  (match "
#team.vlad
#team.imposter
" [:div
   [:p
    "\n"
    [:a {:href "/team.vlad"} "team.vlad"]
    "\n"
    [:a {:href "/team.imposter"} "team.imposter"]]])

  (match
   "* #team.vlad"
    [:div [:ul [:li [:a {:href "/team.vlad"} "team.vlad"]]]])

  (match-inline "@niquola"
                [[:a {:href "/people.niquola"} 'people.niquola]])

  (match-inline "Here is inline code `funciton()`"
                [string? [:code {:class :c-1320827498} "funciton()"]])

  (match-inline "Here is **bold**"
                [string? [:b "bold"]])

  (match-inline "Here is [text](link)"
                [string? [:a {:href "link"} "text"]])

  (match-inline "Here is ![text](link)"
                [string? [:img {:src "link"}]])

  (match-inline "Here is __italic__"
                [string? [:i "italic"]]))

(t/deftest escape-symbols-test
  (def ztx (zen/new-context))
  (match "@foo.bar"
    [:div
     [:p
      "\n"
      [:a
       {:href "/people.foo.bar",
        :class :c1089785874,
        :title "Broken Link"}
       'people.foo.bar]]])

  (match "\\@foo.bar"    [:div [:p "\n" "@foo.bar"]])
  (match "foo\\@foo.bar" [:div [:p "\n" "foo@foo.bar"]])
  (match "\\"            [:div [:p "\n" "\\"]])

  (match "foo@foo.bar"   [:div [:p "\n" "foo@foo.bar"]]))
