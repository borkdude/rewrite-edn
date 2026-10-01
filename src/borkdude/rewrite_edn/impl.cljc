(ns ^:no-doc borkdude.rewrite-edn.impl
  (:refer-clojure :exclude [get assoc update assoc-in update-in dissoc keys
                            get-in conj fnil])
  (:require
   [clojure.core :as c]
   [clojure.string :as str]
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

(defn next-key [zloc]
  (let [v (-> zloc z/right skip-right)]
    (if (z/rightmost? v)
      v
      (-> v z/right skip-right))))

(defn skip-right-to-last-non-ws [zloc]
  (z/skip z/left* z/whitespace? (z/rightmost* zloc)))

(defn enter-namespaced-map [zloc]
  (if (= :namespaced-map (z/tag zloc))
    (-> zloc z/down z/rightmost)
    zloc))

(defn map-qualifier [zloc]
  (when-let [parent (z/up zloc)]
    (when (= :namespaced-map (z/tag parent))
      (-> parent z/down z/node))))

(defn qualify-key [qualifier k]
  (if (and qualifier (or (keyword? k) (symbol? k)))
    (let [ctor (if (keyword? k) keyword symbol)
          k-ns (namespace k)]
      (cond
        (nil? k-ns) (ctor "_" (name k))
        (and (not (:auto-resolved? qualifier))
             (= k-ns (:prefix qualifier))) (ctor (name k))
        :else k))
    k))

(defn representable-key? [qualifier k]
  (not (and qualifier
            (or (keyword? k) (symbol? k))
            (= "_" (namespace k)))))

(defn significant? [node]
  (not (or (node/whitespace-or-comment? node)
           (= :uneval (node/tag node)))))

(defn line-start? [zloc]
  (#{:newline :comment} (some-> (z/left* zloc) z/tag)))

(defn indent-line [zloc n]
  (if (= :whitespace (z/tag zloc))
    (let [width (+ (count (node/string (z/node zloc))) n)]
      (if (pos? width)
        (z/replace* zloc (node/spaces width))
        (z/remove* zloc)))
    (cond-> zloc
      (and (pos? n) (not= :newline (z/tag zloc)))
      (z/insert-left* (node/spaces n)))))

(defn indent [node n]
  (if (zero? n)
    node
    (loop [zloc (z/of-node* node)]
      (if (z/end? zloc)
        (z/root zloc)
        (recur (z/next* (cond-> zloc
                          (line-start? zloc) (indent-line n))))))))

(defn end-col [col s]
  (let [i (str/last-index-of s "\n")]
    (if i
      (- (count s) i)
      (+ col (count s)))))

(defn expand-namespaced-map [zloc k]
  (when (:auto-resolved? (map-qualifier zloc))
    (throw (ex-info (str "Can't write key " k " into an auto-resolved namespaced map")
                    {:key k})))
  (let [node (z/node zloc)
        {:keys [row col]} (meta node)
        shift (- col (:col (meta (z/node (z/up zloc)))))
        first-key (first (filter significant? (:children node)))
        dedent-by (if (= row (some-> first-key meta :row)) shift 0)
        children (vec (:children node))
        key-idxs (set (take-nth 2 (filter #(significant? (children %))
                                          (range (count children)))))
        children (loop [i 0
                        col (inc (- col shift))
                        out []]
                   (if (= i (count children))
                     out
                     (let [child (children i)
                           line-start (and (pos? i)
                                           (#{:newline :comment} (node/tag (children (dec i)))))
                           k (when (key-idxs i) (node/sexpr child))
                           pos (meta child)
                           child (cond
                                   (and line-start (= :whitespace (node/tag child)))
                                   (let [width (- (count (node/string child)) dedent-by)]
                                     (when (pos? width) (node/spaces width)))
                                   (or (keyword? k) (symbol? k)) (node/coerce k)
                                   :else child)
                           child (cond-> child
                                   (and child (:col pos)) (-> (indent (- col (:col pos)))
                                                  (with-meta (c/assoc pos :col col))))]
                       (recur (inc i)
                              (if child (end-col col (node/string child)) col)
                              (cond-> out child (c/conj child))))))]
    (-> zloc z/up (z/replace (node/replace-children node children)) z/root)))

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
  (let [zloc (enter-namespaced-map (z/of-node forms))
        tag (z/tag zloc)
        zloc (enter-namespaced-map
              (z/skip z/right (fn [zloc]
                                (let [t (z/tag zloc)]
                                  (not (contains? #{:token :map :vector :namespaced-map} t))))
                      zloc))
        qualifier (map-qualifier zloc)
        k-node (node/coerce (qualify-key qualifier k))
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
      (not (representable-key? qualifier k))
      (assoc* (expand-namespaced-map zloc k) k v)
      empty?
      (-> zloc
          (z/append-child k-node)
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
                (z/insert-right* k-node)
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
                 (if (= :vector tag)
                   (-> zloc z/right skip-right)
                   (next-key zloc)))))))))))

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
  (let [zloc (enter-namespaced-map (z/of-node zloc))
        tag (z/tag zloc)]
    (cond
      (= :map tag)
      (let [node (z/node zloc)
            nil? (and (identical? :token (node/tag node))
                      (nil? (node/sexpr node)))
            zloc (if nil?
                   (z/replace zloc (node/coerce {}))
                   zloc)
            empty? (or nil? (zero? (count-uncommented-children zloc)))
            zloc (z/down zloc)
            zloc (skip-right zloc)]
        (if empty?
          (node/coerce default)
          (loop [key-count 0
                 zloc zloc]
            (if (z/rightmost? zloc)
              (node/coerce default)
              (let [current-k (z/sexpr zloc)]
                (if (= current-k k)
                  (-> zloc (z/right) (skip-right) first)
                  (recur
                   (inc key-count)
                   (next-key zloc))))))))
      (= :set tag)
      (or (some #(when (= k (node/sexpr %)) %)
                (filter significant? (:children (z/node zloc))))
          (node/coerce default))
      (and (= :vector tag) (integer? k))
      (let [coll (some->> (z/down zloc)
                          (iterate z/right)
                          (take-while identity)
                          (remove #(or (node/whitespace-or-comment? %)
                                       (= :uneval (node/tag %)))))]
        (if (< -1 k (count coll))
          (node/coerce (first (nth coll k)))
          (node/coerce default)))
      :else
      (node/coerce default))))

(defn get-in [zloc ks not-found]
  (reduce (fn [zloc k]
            (if (nil? (node/sexpr zloc))
              (node/coerce not-found)
              (let [v (get zloc k ::not-found)]
                (if (= ::not-found (node/sexpr v))
                  (node/coerce not-found)
                  v))))
          zloc ks))

(defn update*
  ([forms k f]
   (update* forms k f nil))
  ([forms k f args]
   (let [zloc (z/of-node forms)
         zloc (enter-namespaced-map
               (z/skip z/right (fn [zloc]
                                 (let [t (z/tag zloc)]
                                   (not (contains? #{:token :map :vector :namespaced-map} t)))) zloc))
         qualifier (map-qualifier zloc)
         k-node (node/coerce (qualify-key qualifier k))
         t (z/tag zloc)
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
     (cond
       (not (representable-key? qualifier k))
       (update* (expand-namespaced-map zloc k) k f args)
       (and empty? (= :vector t))
       (-> zloc
           (z/append-child (node/coerce k))
           (z/append-child (node/coerce nil))
           (z/root)
           (update* k f args))
       empty?
       (-> zloc
           (z/append-child k-node)
           (z/append-child (node/coerce (apply f (node/coerce nil) args)))
           (z/root))
       :else
       (let [[zloc align-to-loc] (if zloc-comment
                                    [(-> zloc z/down* skip-right-to-last-non-ws) (-> zloc-comment z/node meta)]
                                    (let [zloc-first-key (-> zloc z/down skip-right)]
                                      [zloc-first-key (some-> zloc-first-key z/node meta)]))]
         (case t
           :map
           (loop [key-count 0
                  zloc zloc]
             (if (z/rightmost? zloc)
               (-> zloc
                   (z/insert-right* k-node)
                   (indent-or-space key-count align-to-loc)
                   (z/right)
                   (z/insert-right (apply f (node/coerce nil) args))
                   (z/root))
               (let [current-k (z/sexpr zloc)]
                 (if (= current-k k)
                   (let [zloc (-> zloc (z/right) (skip-right))
                         zloc (z/replace zloc (node/coerce (apply f (z/node zloc) args)))]
                     (z/root zloc))
                   (recur (inc key-count) (next-key zloc))))))
           :vector
           (loop [key-count 0
                  zloc zloc]
             (if (= key-count k)
               (let [zloc (z/replace zloc (node/coerce (apply f (z/node zloc) args)))]
                 (z/root zloc))
               (if (z/rightmost? zloc)
                 (throw (ex-info (str "Can't insert at index " k " for vector") {}))
                 (recur (inc key-count)
                        (-> zloc
                            (skip-right)
                            (z/right))))))))))))

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

(defn find-map [zloc]
  (enter-namespaced-map
   (if (#{:map :namespaced-map} (z/tag zloc))
     zloc
     (z/skip z/right (fn [zloc]
                       (and (not (z/rightmost? zloc))
                            (not (#{:map :namespaced-map} (z/tag zloc))))) zloc))))

(declare keys)

(defn replace-keys [new-ks forms]
  (let [map-zloc (find-map (z/of-node forms))
        qualifier (map-qualifier map-zloc)
        zloc (some-> map-zloc z/down skip-right)
        bad-key (when qualifier
                  (some #(when-not (representable-key? qualifier %) %) new-ks))]
    (cond
      bad-key (replace-keys new-ks (expand-namespaced-map map-zloc bad-key))
      (nil? zloc) (z/root map-zloc)
      :else
      (loop [zloc zloc
             new-ks new-ks]
        (if (z/rightmost? zloc)
          (z/root zloc)
          (let [zloc (let [new-key (node/coerce (qualify-key qualifier (first new-ks)))]
                       (-> (z/replace zloc new-key)
                           z/right))]
            (recur (-> zloc
                       ;; move over value to next key
                       (skip-right)
                       maybe-right
                       (skip-right))
                   (rest new-ks))))))))

(defn map-keys [f forms]
  (replace-keys (mapv (comp f node/sexpr) (reverse (keys forms))) forms))

(defn dissoc [forms k]
  (let [zloc (z/of-node forms)
        zloc (enter-namespaced-map
              (z/skip z/right (fn [zloc]
                                (let [t (z/tag zloc)]
                                  (not (contains? #{:token :map :namespaced-map} t)))) zloc))
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
                (let [key-node (z/node zloc)]
                  (-> zloc z/right skip-right z/remove
                      (z/find z/prev #(identical? key-node (z/node %)))
                      z/remove z/root))
                (recur (next-key zloc))))))))))

(defn keys [forms]
  (let [zloc (find-map (z/of-node forms))
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
      (contains? #{:map :namespaced-map} tag) (assoc forms (first v) (second v))
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
