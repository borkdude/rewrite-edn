(ns borkdude.rewrite-edn.impl
  (:refer-clojure :exclude [assoc update assoc-in update-in])
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

(defn indent [zloc key-count first-key-loc]
  (let [current-loc (meta (z/node zloc))]
    (if (or (= 1 key-count)
            (not= (:row first-key-loc) (:row current-loc)))
      (let [zloc (-> zloc
                     (z/insert-space-right (dec (dec (:col first-key-loc))))
                     z/insert-newline-right)]
        zloc)
      zloc)))

(defn assoc
  [forms k v]
  (let [zloc (z/edn forms)
        zloc (z/skip z/right (fn [zloc] (not= :map (z/tag zloc))) zloc)
        empty? (zero? (count (:children (z/node zloc))))]
    (if empty?
      (-> zloc
          (z/append-child (node/coerce k))
          (z/append-child (node/coerce v))
          (z/root))
      (let [zloc (z/down zloc)
            zloc (skip-right zloc)
            ;; the location of the first key:
            first-key-loc (when-let [first-key (z/node zloc)]
                            (meta first-key))]
        (loop [key-count 0
               zloc zloc]
          (if (z/rightmost? zloc)
            (-> zloc
                (z/insert-right (node/coerce k))
                (indent key-count first-key-loc)
                (z/right)
                (z/insert-right (node/coerce v))
                (z/root))
            (let [current-k (z/sexpr zloc)]
              (if (= current-k k)
                (let [zloc (-> zloc (z/right) (skip-right))
                      zloc (z/replace zloc (node/coerce v))]
                  (z/root zloc))
                (recur
                 (inc key-count)
                 (-> zloc
                     ;; move over value to next key
                     (skip-right)
                     (z/right)
                     (skip-right)))))))))))

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

(defn update-in [forms keys f]
  (if (= 1 (count keys))
    (update forms (first keys) f)
    (update forms (first keys) #(update-in % (rest keys) f))))

(defn assoc-in [forms keys v]
  (if (= 1 (count keys))
    (assoc forms (first keys) v)
    (update forms (first keys) #(assoc-in % (rest keys) v))))

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
