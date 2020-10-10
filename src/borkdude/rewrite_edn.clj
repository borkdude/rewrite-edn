(ns borkdude.rewrite-edn
  (:refer-clojure :exclude [assoc])
  (:require [borkdude.rewrite-edn.impl :as impl]
            [clojure.core :as c]
            [rewrite-cljc.node :as node]))

(defprotocol ToNode
  (to-node [this]))

(extend-protocol ToNode
  Object
  (to-node [this] (node/token-node this))

  clojure.lang.Keyword
  (to-node [this] (node/keyword-node this)))

(defn assoc [node k v]
  (impl/assoc node k v))
