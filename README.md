# zd (zen docs)

Semantic knowledge base


* Knowledge base consists of structured resources.
* Each resource is file in zd format.
* Full name of resource calculated from project root dirictory: /myns/resource.zd => myns.resource
and may be referenced from other resources
* Resource may be described with zen tags


zd format is sugar for edn:

```edn
:zd/title "Person"
:zen/tags #{foam.Person}
;; specila multiline string
:zd/desc md/
# Title
here is 

* any markdown

;; nested datastructure can be flatten with ~
:zd/book~:intro~
~:title "Title"
~:text md/

This is intro

:zd/book~:examples~
~:title "Example"
~:text md/

;; There is special syntax for vectors
:zd/comments~#~
~:author aidbox.team.niquola
~:date "2021-11-11"
~:text md/

Comment 1

:zd/comments~#~
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

* syntax highlight
* static site generator
* custom markdown with symbolic links
* direct hickup support
* plugins
