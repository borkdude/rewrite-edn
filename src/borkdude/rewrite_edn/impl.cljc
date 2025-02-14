(ns ^:no-doc borkdude.rewrite-edn.impl
  (:refer-clojure :exclude [get assoc update assoc-in update-in dissoc keys
                            get-in conj fnil])
  (:require
   [clojure.core :as c]
   [rewrite-clj.node :as node]
   [rewrite-clj.parser :as p]
   [rewrite-clj.zip :as z]))

(defn count-uncommented-children [zloc]
  (->> (z/node zloc)
       :children
       (remove
         #(or (node/whitespace-or-comment? %)
             (= :uneval (node/tag %))))
       count))

(defn comment-loc? [zloc]
  (#{:comment :uneval} (z/tag zloc)))

(defn find-comment-child-loc [zloc]
  (some-> zloc
          z/down*
          (z/find z/right* comment-loc?)))

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

(defn skip-right-to-last-non-ws [zloc]
  (z/skip z/left* z/whitespace? (z/rightmost* zloc)))

(defn indent-or-space [zloc key-count align-loc]
  (let [current-loc (meta (z/node zloc))]
    (if (and align-loc
             (or (= 1 key-count)
                 (comment-loc? zloc)
                 (not= (:row align-loc) (:row current-loc))))
      (let [indent-spaces (dec (:col align-loc))]
        (cond-> zloc
          (pos? indent-spaces)
          (z/insert-space-right indent-spaces)
          (not= :comment (z/tag zloc))
          z/insert-newline-right))
      (-> zloc
          (z/insert-space-right 1)))))

(defn assoc*
  [forms k v]
  (let [zloc (z/of-node forms)
        tag (z/tag zloc)
        zloc (z/skip z/right (fn [zloc]
                               (let [t (z/tag zloc)]
                                 (not (contains? #{:token :map :vector} t))))
                     zloc)
        node (z/node zloc)
        nil? (and (identical? :token (node/tag node))
                  (nil? (node/sexpr node)))
        zloc (if nil?
               (z/replace zloc (node/coerce {}))
               zloc)
        length (count-uncommented-children zloc)
        out-of-bounds? (and (= :vector tag) (>= k length))
        zloc-comment (when (zero? length) (find-comment-child-loc zloc))
        empty? (and (or nil? (zero? length))
                    (not zloc-comment))]
    (cond
      empty?
      (-> zloc
          (z/append-child (node/coerce k))
          (z/append-child (node/coerce v))
          (z/root))
      out-of-bounds?
      (throw #?(:clj (java.lang.IndexOutOfBoundsException.)
                :cljs (ex-info "IndexOutOfBounds" {})))
      :else
      (let [[zloc align-to-loc] (if zloc-comment
                                  [(-> zloc z/down* skip-right-to-last-non-ws) (-> zloc-comment z/node meta)]
                                  (let [zloc-first-key (-> zloc z/down skip-right)]
                                    [zloc-first-key (some-> zloc-first-key z/node meta)]))]
        (loop [key-count 0
               zloc zloc]
          (if (and (#{:token :map} tag) (z/rightmost? zloc))
            (-> zloc
                (z/insert-right* (node/coerce k))
                (indent-or-space key-count align-to-loc)
                (z/right)
                (z/insert-right (node/coerce v))
                (z/root))
            (let [current-k (z/sexpr zloc)]
              (cond
                (and (= :vector tag)
                     (= key-count k))
                (let [zloc (z/replace zloc (node/coerce v))]
                  (z/root zloc))
                (and (#{:token :map} tag)
                     (= current-k k))
                (let [zloc (-> zloc (z/right) (skip-right))
                      zloc (z/replace zloc (node/coerce v))]
                  (z/root zloc))
                :else
                (recur
                 (inc key-count)
                 (-> zloc
                     ;; move over value to next key
                     (skip-right)
                     (z/right)
                     (skip-right)))))))))))

(defn mark-for-positional-recalc [node]
  (vary-meta node c/assoc :rewrite-edn/positional-recalc true))

(defn recalc-positional-metadata [node]
  (if (:rewrite-edn/positional-recalc (meta node))
    (-> node
        str
        p/parse-string-all)
    node))

(defn assoc [forms k v]
  (-> (recalc-positional-metadata forms)
      (assoc* k v)
      mark-for-positional-recalc))

(defn get [zloc k default]
  (let [zloc (z/of-node zloc)
        tag (z/tag zloc)]
    (cond
      (= :map tag)
      (let [node (z/node zloc)
            nil? (and (identical? :token (node/tag node))
                      (nil? (node/sexpr node)))
            zloc (if nil?
                   (z/replace zloc (node/coerce {}))
                   zloc)
            empty? (or nil? (zero? (count (:children (z/node zloc)))))
            zloc (z/down zloc)
            zloc (skip-right zloc)]
        (if empty?
          :empty
          (loop [key-count 0
                 zloc zloc]
            (if (z/rightmost? zloc)
              (node/coerce default)
              (let [current-k (z/sexpr zloc)]
                (if (= current-k k)
                  (-> zloc (z/right) (skip-right) first)
                  (recur
                   (inc key-count)
                   (-> zloc
                       (skip-right)
                       (z/right)
                       (skip-right)))))))))
      :else
      (let [coll (some->> (z/down zloc)
                          (iterate z/right)
                          (take-while identity)
                          (remove #(or (node/whitespace-or-comment? %)
                                       (= :uneval (node/tag %)))))]
        (if (>= k (count coll))
          (node/coerce default)
          (node/coerce (first (nth coll k))))))))

(defn get-in [zloc ks not-found]
  (reduce (fn [zloc k]
            (if (nil? (node/sexpr zloc))
              (node/coerce not-found)
              (let [v (get zloc k ::not-found)]
                (if (or (= :empty v)
                        (= ::not-found (node/sexpr v)))
                  (node/coerce not-found)
                  v))))
          zloc ks))

(defn update*
  ([forms k f]
   (update* forms k f nil))
  ([forms k f args]
   (let [zloc (z/of-node forms)
         zloc (z/skip z/right (fn [zloc]
                                (let [t (z/tag zloc)]
                                  (not (contains? #{:token :map} t)))) zloc)
         node (z/node zloc)
         nil? (and (identical? :token (node/tag node))
                   (nil? (node/sexpr node)))
         length (count-uncommented-children zloc)
         zloc (if nil?
                (z/replace zloc (node/coerce {}))
                zloc)
         zloc-comment (when (zero? length) (find-comment-child-loc zloc))
         empty? (and (or nil? (zero? length))
                     (not zloc-comment))]
     (if empty?
       (-> zloc
           (z/append-child (node/coerce k))
           (z/append-child (node/coerce nil))
           (z/root)
           (update* k f args))
       (let [[zloc align-to-loc] (if zloc-comment
                                    [(-> zloc z/down* skip-right-to-last-non-ws) (-> zloc-comment z/node meta)]
                                    (let [zloc-first-key (-> zloc z/down skip-right)]
                                      [zloc-first-key (some-> zloc-first-key z/node meta)]))]
         (loop [key-count 0
                zloc zloc]
           (if (z/rightmost? zloc)
             (-> zloc
                 (z/insert-right* (node/coerce k))
                 (indent-or-space key-count align-to-loc)
                 (z/right)
                 (z/insert-right (apply f (node/coerce nil) args))
                 (z/root))
             (let [current-k (z/sexpr zloc)]
               (if (= current-k k)
                 (let [zloc (-> zloc (z/right) (skip-right))
                       zloc (z/replace zloc (node/coerce (apply f (z/node zloc) args)))]
                   (z/root zloc))
                 (recur (inc key-count)
                        (-> zloc
                            ;; move over value to next key
                            (skip-right)
                            (z/right)
                            (skip-right))))))))))))

(defn update
  ([forms k f]
   (update forms k f nil))
  ([forms k f args]
   (-> (recalc-positional-metadata forms)
       (update* k f args)
       mark-for-positional-recalc)))

(defn update-in [forms keys f args]
  (-> (if (= 1 (count keys))
        (update forms (first keys) f args)
        (update forms (first keys) #(update-in % (rest keys) f args)))
      (mark-for-positional-recalc)))

(defn assoc-in [forms keys v]
  (if (= 1 (count keys))
    (assoc forms (first keys) v)
    (-> (recalc-positional-metadata forms)
        (update (first keys) #(assoc-in % (rest keys) v))
        (mark-for-positional-recalc))))

(defn map-keys [f forms]
  (let [zloc (z/of-node forms)
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

(defn dissoc [forms k]
  (let [zloc (z/of-node forms)
        zloc (z/skip z/right (fn [zloc]
                               (let [t (z/tag zloc)]
                                 (not (contains? #{:token :map} t)))) zloc)
        node (z/node zloc)
        nil? (and (identical? :token (node/tag node))
                  (nil? (node/sexpr node)))]
    (if nil?
      forms
      (let [zloc (z/down zloc)
            zloc (skip-right zloc)]
        (loop [zloc zloc]
          (if (z/rightmost? zloc)
            forms
            (let [current-k (z/sexpr zloc)]
              (if (= current-k k)
                (-> zloc z/right z/remove
                    z/remove z/root)
                (recur (-> zloc
                           ;; move over value to next key
                           (skip-right)
                           (z/right)
                           (skip-right)))))))))))

(defn keys [forms]
  (let [zloc (z/of-node forms)
        zloc (if (= :map (z/tag zloc))
               zloc
               (z/skip z/right (fn [zloc]
                                 (and (not (z/rightmost zloc))
                                      (not= :map (z/tag zloc)))) zloc))
        zloc (z/down zloc)
        zloc (skip-right zloc)]
    (loop [zloc zloc
           ks '()]
      (if (z/rightmost? zloc)
        ks
        (let [k (z/node zloc)]
          (recur (-> zloc
                     ;; move over value to next key
                     z/right
                     (skip-right)
                     maybe-right
                     (skip-right))
                 (c/conj ks k)))))))

(defn conj* [forms v]
  (let [zloc (z/of-node forms)
        node (z/node zloc)
        tag  (node/tag node)
        nil? (and (identical? :token tag)
                  (nil? (node/sexpr node)))]
    (cond
      nil? (-> (z/replace zloc (node/coerce (list v)))
               (z/root))
      (contains? #{:vector :set} tag) (-> (z/append-child zloc (node/coerce v))
                                          (z/root))
      (identical? tag :list) (-> zloc
                                 (z/insert-child (node/coerce v))
                                 (z/root))
      (identical? tag :map) (assoc forms (first v) (second v))
      :else
      (throw (ex-info "Unsupported forms" {:forms forms})))))

(defn conj [forms v]
  (-> (recalc-positional-metadata forms)
      (conj* v)
      mark-for-positional-recalc))

(defn fnil [f nil-replacement]
  (fn [x & args]
    (if (= "nil" (str x))
      (apply f (node/coerce nil-replacement) args)
      (apply f x args))))
