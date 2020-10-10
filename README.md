# rewrite-edn

Utility lib on top of
[rewrite-cljc](https://github.com/lread/rewrite-cljc-playground) with common
operations to update EDN while preserving whitespace and comments.

## API

Currently implemented:

- `parse-string`: parses EDN into nodes
- `assoc`: associates `k` and `v` into first map node found in nodes. Coerces `k` and `v` into node.

## Examples

An example from the tests:

```
(deftest assoc-test
  (is (= "{:a 1 :b 1}"
         (str (r/assoc
               (r/parse-string "{:a 1}")
               :b 1))))
  (is (= "{:a 2}"
         (str (r/assoc
               (r/parse-string "{:a 1}")
               :a 2))))
  (is (= "{:a #_:something 2}"
         (str (r/assoc
               (r/parse-string "{:a #_:something 1}")
               :a 2))))
  (is (= "{:a 2} ;; this is a cool map"
         (str (r/assoc
               (r/parse-string "{:a 1} ;; this is a cool map")
               :a 2)))))
```

# License

Copyright © 2020 Michiel Borkent

Distributed under the EPL License, same as Clojure. See LICENSE.
