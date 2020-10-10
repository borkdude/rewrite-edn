(ns borkdude.rewrite-edn.impl
  (:refer-clojure :exclude [assoc update])
  (:require [rewrite-cljc.node :as node]
            [rewrite-cljc.zip :as z]))

(defn maybe-right [zloc]
  (if (z/rightmost? zloc)
    zloc
    (z/right zloc)))

(defn skip-right [zloc]
  (z/skip z/right
          (fn [zloc]
            (and
             (not (z/rightmost? zloc))
             (or (node/whitespace-or-comment? (z/node zloc))
                 (= :uneval (z/tag zloc)))))
          zloc))

(defn prepend-node [zloc {:keys [:newline]} loc]
  (if newline
    (let [zloc (-> zloc
                   (z/insert-space-right (dec (dec (:col loc))))
                   z/insert-newline-right)]
      zloc)
    zloc))

(defn assoc
  [forms k v opts]
  (let [zloc (z/edn forms)
        zloc (z/skip z/right (fn [zloc] (not= :map (z/tag zloc))) zloc)
        zloc (z/down zloc)
        zloc (skip-right zloc)
        loc (meta (z/node zloc))]
    (loop [zloc zloc]
      (if (z/rightmost? zloc)
        (-> zloc
            (z/insert-right (node/coerce k))
            (prepend-node opts loc)
            (z/right)
            (z/insert-right (node/coerce v))
            (z/root))
        (let [current-k (z/sexpr zloc)]
          (if (= current-k k)
            (let [zloc (-> zloc (z/right) (skip-right))
                  zloc (z/replace zloc (node/coerce v))]
              (z/root zloc))
            (recur (-> zloc
                       ;; move over value to next key
                       (skip-right)
                       (z/right)
                       (skip-right)))))))))

(defn update [forms k f]
  (let [zloc (z/edn forms)
        zloc (z/skip z/right (fn [zloc] (not= :map (z/tag zloc))) zloc)
        zloc (z/down zloc)
        zloc (skip-right zloc)]
    (loop [zloc zloc]
      (if (z/rightmost? zloc)
        (-> zloc
            (z/insert-right (node/coerce k))
            (z/right)
            (z/insert-right (f nil))
            (z/root))
        (let [current-k (z/sexpr zloc)]
          (if (= current-k k)
            (let [zloc (-> zloc (z/right) (skip-right))
                  zloc (z/replace zloc (node/coerce (f (z/node zloc))))]
              (z/root zloc))
            (recur (-> zloc
                       ;; move over value to next key
                       (skip-right)
                       (z/right)
                       (skip-right)))))))))

(defn map-keys [f forms]
  (let [zloc (z/edn forms)
        zloc (if (= :map (z/tag zloc))
               zloc
               (z/skip z/right (fn [zloc]
                                 (and (not (z/rightmost zloc))
                                      (not= :map (z/tag zloc)))) zloc))
        zloc (z/down zloc)
        zloc (skip-right zloc)]
    (loop [zloc zloc]
      (if (z/rightmost? zloc)
        (z/root zloc)
        (let [zloc (let [new-key (node/coerce (f (z/sexpr zloc)))]
                     (-> (z/replace zloc new-key)
                         z/right))]
          (recur (-> zloc
                     ;; move over value to next key
                     (skip-right)
                     maybe-right
                     (skip-right))))))))
