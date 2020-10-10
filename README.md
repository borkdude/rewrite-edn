# rewrite-edn

Utility lib on top of
[rewrite-cljc](https://github.com/lread/rewrite-cljc-playground) with common
operations to update EDN while preserving whitespace and comments.

## API

Currently implemented. See docstrings in the `borkdude.rewrite-edn` namespace for details.

- `parse-string`
- `assoc`
- `update`

## Examples

An example: fully qualify deps symbols in `deps.edn`.

Given `deps.edn`:

``` clojure
{:deps {foo {:mvn/version "0.1.0"}
        bar {:mvn/version "0.2.0"}
        ;; here's a comment and the next dep is ignored:
        #_baz #_{:mvn/version "0.3.0"}}}
```

and this script:

``` clojure
(require '[borkdude.rewrite-edn :as r])

(def edn-string (slurp "deps.edn"))

(def nodes (r/parse-string edn-string))

(println "Before:")
(println (str nodes))

(defn qualify-sym-node [sym-node]
  (let [sym (r/sexpr sym-node)]
    (if (or (not (symbol? sym))
            (qualified-symbol? sym))
      sym-node
      (symbol (str sym) (str sym)))))

(def updated-nodes (r/update nodes :deps #(r/map-keys qualify-sym-node %)))
(println "After:")
(println (str updated-nodes))
```

it prints:

``` clojure
Before:
{:deps {foo {:mvn/version "0.1.0"}
        bar {:mvn/version "0.2.0"}
        ;; here's a comment and the next dep is ignored:
        #_baz #_{:mvn/version "0.3.0"}}}

After:
{:deps {foo/foo {:mvn/version "0.1.0"}
        bar/bar {:mvn/version "0.2.0"}
        ;; here's a comment and the next dep is ignored:
        #_baz #_{:mvn/version "0.3.0"}}}
```

# License

Copyright © 2020 Michiel Borkent

Distributed under the EPL License, same as Clojure. See LICENSE.
