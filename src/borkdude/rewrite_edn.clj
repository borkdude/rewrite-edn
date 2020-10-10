(ns borkdude.rewrite-edn
  (:refer-clojure :exclude [assoc zipmap update])
  (:require [borkdude.rewrite-edn.impl :as impl]
            [clojure.core :as c]
            [rewrite-cljc.parser :as p]))

(defn parse-string [s]
  (p/parse-string-all s))

(defn assoc [node k v]
  (impl/assoc node k v))

(defn update [node k f]
  (impl/update node k f))

(defn map-keys [f node]
  (impl/map-keys f node))
