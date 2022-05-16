ClojureScript linter POC based on cljs.analyzer

```clojure
(future
  (linter.core/with-linter
    {:build-tool :cljs.main
     :plugins {:react/hooks {}}}
    (cljs.main/-main "-w" "dev" "-c" "linter.test")))
```
