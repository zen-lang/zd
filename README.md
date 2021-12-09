# zd (zen docs)

Semantic knowledge base


Knowledge base consists of structured resources.
Each resource is file in zd format.
Full name of resource calculated from project root dirictory: /myns/resource.zd => myns.resource
and may be referenced from other resources

zd format is sugar for edn:

```edn
:zd/title "Person"
:zen/tags #{foam.Person}
;; specila multiline string
:zd/desc md/
# Title
here is 

* any markdown
```
