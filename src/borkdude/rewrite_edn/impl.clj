(ns borkdude.rewrite-edn.impl
  (:refer-clojure :exclude [assoc])
  (:require [rewrite-cljc.node :as node]
            [rewrite-cljc.zip :as z]))

(defn skip [zloc]
  (z/skip z/right
          (fn [zloc]
            (or (node/whitespace-or-comment? (z/node zloc))
                (= :uneval (z/tag zloc))))
          zloc))

(defn assoc [forms k v]
  (let [zloc (z/edn forms)
        zloc (z/skip z/right (fn [zloc] (not= :map (z/tag zloc))) zloc)
        zloc (z/down zloc)
        zloc (skip zloc)]
    (loop [zloc zloc]
      (if (z/rightmost? zloc)
        (-> zloc
            (z/insert-right (node/coerce k))
            (z/right)
            (z/insert-right (node/coerce v))
            (z/root))
        (let [current-k (z/sexpr zloc)]
          (if (= current-k k)
            (let [zloc (-> zloc (z/right) (skip))
                  zloc (z/replace zloc (node/coerce v))]
              (z/root zloc))
            (recur (-> zloc
                       ;; move over value to next key
                       (skip)
                       (z/right)
                       (skip)))))))))
