(ns borkdude.rewrite-edn
  (:refer-clojure :exclude [assoc assoc-in update update-in])
  (:require [borkdude.rewrite-edn.impl :as impl]
            [clojure.core :as c]
            [rewrite-cljc.node :as node]
            [rewrite-cljc.parser :as p]))

(defn parse-string
  "Same as rewrite-cljc.parser/parse-string-all"
  [s]
  (p/parse-string-all s))

(defn sexpr
  "Same as rewrite-cljc.node/sexpr"
  [node]
  (node/sexpr node))

(defn assoc
  "Associates k to v in node (which may be a forms node as returned by parse-string or map node).
  Both k and v are coerced into nodes."
  ([node k v]
   (impl/assoc node k v)))

(defn assoc-in
  "Associates value under keys ks in map node with v."
  [node ks v]
  (impl/assoc-in node ks v))

(defn update
  "Updates value under key k in map node. Function f receives
  node. Return value is coerced into node."
  [node k f]
  (impl/update node k f))

(defn update-in
  "Updates value under keys ks in map node. Function f receives
  node. Return value is coerced into node."
  [node ks f]
  (impl/update-in node ks f))

(defn map-keys
  "Maps f over keys of node (which may be a forms node as returned by
  parse-string or map node)."
  [f node]
  (impl/map-keys f node))
