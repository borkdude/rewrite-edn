(ns borkdude.rewrite-edn
  (:refer-clojure :exclude [assoc assoc-in update update-in dissoc get keys
                            get-in])
  (:require [borkdude.rewrite-edn.impl :as impl]
            [clojure.core :as c]
            [rewrite-clj.node :as node]
            [rewrite-clj.parser :as p]))

(defn parse-string
  "Same as rewrite-clj.parser/parse-string-all"
  [s]
  (p/parse-string-all s))

(defn sexpr
  "Same as rewrite-clj.node/sexpr"
  [node]
  (node/sexpr node))

(defn assoc
  "Associates k to v in node (which may be a forms node as returned by parse-string or map node).
  Both k and v are coerced into nodes."
  ([node k v]
   (impl/assoc node k v)))

(defn get
  "Returns the value mapped to k, default or nil if key not present."
  ([node k] (get node k (node/coerce nil)))
  ([node k default]
   (impl/get node k default)))

(defn get-in
  "Returns the value in a nested associative structure,
  where ks is a sequence of keys. Returns nil if the key
  is not present, or the not-found value if supplied."
  ([node ks] (get-in node ks (node/coerce nil)))
  ([node ks not-found]
   (impl/get-in node ks not-found)))

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

(defn dissoc
  [node k]
  (impl/dissoc node k))

(defn keys
  [node]
  (impl/keys node))
