# Changelog

[Rewrite-edn](https://github.com/borkdude/rewrite-edn): utility lib on top of
[rewrite-clj](https://github.com/clj-commons/rewrite-clj) with common operations
to update EDN while preserving whitespace and comments.

## 0.4.9

- [#40](https://github.com/borkdude/rewrite-edn/issues/40): `assoc`/`update` now handles map keys that have no indent at all ([@lread](https://github.com/lread))
- [#43](https://github.com/borkdude/rewrite-edn/issues/43): bump rewrite-clj to 1.1.49 ([@lread](https://github.com/lread))
- [#40](https://github.com/borkdude/rewrite-edn/issues/40): `assoc`/`update` now handles map keys that have no indent at all 
([@lread](https://github.com/lread))
- [#40](https://github.com/borkdude/rewrite-edn/issues/40): `assoc`/`update` now aligns indent to comment if that's all that is in the map
([@lread](https://github.com/lread))
- [#40](https://github.com/borkdude/rewrite-edn/issues/40): `update` now indents new entries in same way as `assoc`
([@lread](https://github.com/lread))

## 0.4.8

- Add newline after adding new element to top level map with `assoc-in`

## 0.4.7

- [#35](https://github.com/borkdude/rewrite-edn/issues/35): Bump rewrite-clj to 1.1.47

## 0.4.6

- [#33](https://github.com/borkdude/rewrite-edn/issues/33): fix `assoc` on map starting with comment

## 0.4.5

- Add `conj`: `(str (r/update (r/parse-string "{:a [1 2 3]}") :a r/conj 1)) ;;=> "{:a [1 2 3 1]}"` ([@zerg000000](https://github.com/zerg000000))
- Add `fnil`: `(str (r/update (r/parse-string "{:a [1 2 3]}") :b (r/fnil r/conj []) 1)) ;;=> "{:a [1 2 3] :b [1]}"`

## 0.3.4

- [#20](https://github.com/borkdude/rewrite-edn/issues/20): Bump rewrite-clj to v1.1.45 ([@lread](https://github.com/lread))
- [#19](https://github.com/borkdude/rewrite-edn/issues/19): Repeated `assoc-in`, `assoc` no longer throw `NullPointerException` ([@lread](https://github.com/lread))
- Add `keys` and `get-in` ([@witek](https://github.com/witek))

## 0.2.0

- Add `get`
- Improvements for `assoc`

## 0.1.0

- Change `map-vals` to `map-keys`

## 0.0.2

- Fix NPE in `assoc-in`

## 0.0.1

Initial release
