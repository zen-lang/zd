# zd (zen docs)


Semantic knowledge base engine.

* every document is a data with qualified name
* all documents is your database
* access database from your document
* annotate data for representation
* add schemas

Every document is a data. Let's call it resource.  Resource is a ordered set of keys and values (map in clojure terms).
More precisely set of keypaths and values

Top level keys starts on new line from ':' and value in EDN after

```
:this-is-a-key "value"
```

To flatten nested structure keypath expression may be used - keys separated with `#`
which means insert value into resource at path `[:key, :nested-key]`

```
:name#:given "???"
:name#:family "???"
;; same as
:name {:given "???" :family "???"}

```

Each keypath may have annotations (metadata).
Annotation starts with '^' following name and optional EDN value after

```
^table {:columns [:id]}
:items [{:id "p1"}, {:id "p2"}]

```

For multi-line strings there is special notation, which starts form `:key [content-type]/`
and ends at newline with keypath or annotation (i.e. ^: or ^^)

```
:content markdown/

Here is some markdown
* item 1

:another-key "..."


```

Parser read the document and builds two data one as data and second for 



## zd syntax

* Keypath
* Data
* Multiline string
* Annotations


## Setup zen docs for your project

You have to have installed clojure cli. Go to the root of your project and run: 

```
echo ':title "My first zd/note 🙂"' > docs/readme.zd

clojure -Sdeps '{:deps {zen-lang/zd {:git/url "git@github.com:zen-lang/zd.git" :sha "53ca914e73ff9cf6db75434d5d6dafe425e2b057"}}}' -M -m zd.dev docs zrc
```

Open http://localhost:3030 and that's it. 

## Syntax

zd format is sugar for edn:

```edn
:title "Person"
:zen/tags #{foam.Person}
;; specila multiline string
:zd/desc md/
# Title
here is 

* any markdown

;; nested datastructure can be flatten with ~
:zd/book:intro
~:title "Title"
~:text md/

This is intro

:zd/book:examples
~:title "Example"
~:text md/

;; There is special syntax for vectors
:zd/comments:#
~:author aidbox.team.niquola
~:date "2021-11-11"
~:text md/

Comment 1

:zd/comments:#
~:author aidbox.team.niquola
~:date "2021-11-11"
~:text md/

Comment 2


Examples

;; direct hickup support


:aidbox/report
[:ul
 (->> (zd/by-tag 'incidents)
      (group-by :inci/customer))]
```

## TODO

^Eugeny&Max
* zd-mode for emacs
 * syntax
 * jump

^Vlad&Nicola
* markdown/zendown extensible down 
  * [[multi params params]]
  * code blocks with hl

* zd api (search)

* macro engine

* zen schema integration

* hickup engine
  * expressions
  * helpers - zd api search 
  * render helpers tables

^Eugeny&Max
* static generator
  * ui styles
  * render blocks  (annotations)
  * navigation (dsl & render)  

* dev-mode (watch file -> reload resource in db) -> render page

* plugin example
* import plugin

* sample knowledge base


```
git clone kb
cd kb
zd dev-mode

open localhost:3030

edit file.zd

reloaded automaticaly localhost:3030

git push

github actions
github pages

```


TODO:

* hickup keypathes - eval - search/filters <- niquola (stylization)
* search  <- apricotlace 
* emacs <- max
* zen integration + show errors (errors in menu) <- vlad


* Capitilize for headers (overide header)
* sytles - list styles
* table block
* backrefs - sytle 


* custom menu
* search
* devmode 

20:00 pm

Demo

* existing
* how to make knowledge base + how to extend (show on rpc)

## vscode

Publish https://code.visualstudio.com/api/working-with-extensions/publishing-extension

```
vsce login zendoc-vs
vsce publish

```
