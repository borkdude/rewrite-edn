# rewrite-edn

[![Clojars Project](https://img.shields.io/clojars/v/borkdude/rewrite-edn.svg)](https://clojars.org/borkdude/rewrite-edn)
[![bb compatible](https://raw.githubusercontent.com/babashka/babashka/master/logo/badge.svg)](https://book.babashka.org#badges)

Utility lib on top of
[rewrite-clj](https://github.com/clj-commons/rewrite-clj) with common
operations to update EDN while preserving whitespace and comments.

## API

See docstrings in the `borkdude.rewrite-edn` namespace for details.

Currently implemented:

- `parse-string`
- `sexpr`
- `get`
- `keys`
- `assoc`
- `assoc-in`
- `update`
- `update-in`
- `dissoc`
- `map-keys`
- `conj`

## Examples

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
```

### Add dependency

``` clojure
(println (str (r/assoc-in nodes [:deps 'my-other-dep] {:mvn/version "0.1.2"})))
```

``` clojure
{:deps {foo {:mvn/version "0.1.0"}
        bar {:mvn/version "0.2.0"}
        ;; here's a comment and the next dep is ignored:
        #_baz #_{:mvn/version "0.3.0"}
        my-other-dep {:mvn/version "0.1.2"}}}
```

### Fully qualify dep symbols

``` clojure
(defn qualify-sym-node [sym-node]
  (let [sym (r/sexpr sym-node)]
    (if (or (not (symbol? sym))
            (qualified-symbol? sym))
      sym-node
      (symbol (str sym) (str sym)))))

(def updated-nodes (r/update nodes :deps #(r/map-keys qualify-sym-node %)))
(println (str updated-nodes))
```

``` clojure
{:deps {foo/foo {:mvn/version "0.1.0"}
        bar/bar {:mvn/version "0.2.0"}
        ;; here's a comment and the next dep is ignored:
        #_baz #_{:mvn/version "0.3.0"}}}
```

### Conj + fnil

``` clojure
(str (r/update (r/parse-string "{:a [1 2 3]}") :b (r/fnil r/conj []) 1))
;;=> "{:a [1 2 3] :b [1]}
```

Also see [examples](examples).

# License

Copyright © 2021 - 2022 Michiel Borkent

Distributed under the EPL License, same as Clojure. See LICENSE.
