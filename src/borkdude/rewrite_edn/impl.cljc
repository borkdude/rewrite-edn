(ns ^:no-doc borkdude.rewrite-edn.impl
  (:refer-clojure :exclude [get assoc update assoc-in update-in dissoc keys
                            get-in conj fnil])
  (:require
   [clojure.core :as c]
   [clojure.string :as str]
   [rewrite-clj.node :as node]
   [rewrite-clj.parser :as p]
   [rewrite-clj.zip :as z]))

(defn significant? [node]
  (not (or (node/whitespace-or-comment? node)
           (= :uneval (node/tag node)))))

(defn significant-children [zloc]
  (filterv significant? (:children (z/node zloc))))

(defn nil-node? [node]
  (and (= :token (node/tag node))
       (nil? (node/sexpr node))))

(defn comment-loc? [zloc]
  (#{:comment :uneval} (z/tag zloc)))

(defn skip-right [zloc]
  (z/skip z/right
          #(and (not (z/rightmost? %))
                (not (significant? (z/node %))))
          zloc))

(defn enter-namespaced-map [zloc]
  (if (= :namespaced-map (z/tag zloc))
    (-> zloc z/down z/rightmost)
    zloc))

(defn find-target [forms tags]
  (enter-namespaced-map
   (z/skip z/right #(not (tags (z/tag %))) (z/of-node forms))))

(defn map-qualifier [zloc]
  (let [parent (z/up zloc)]
    (when (= :namespaced-map (z/tag parent))
      (-> parent z/down z/node))))

(defn qualify-key [qualifier k]
  (if (and qualifier (ident? k))
    (let [ctor (if (keyword? k) keyword symbol)
          k-ns (namespace k)]
      (cond
        (nil? k-ns) (ctor "_" (name k))
        (= k-ns (:prefix qualifier)) (ctor (name k))
        :else k))
    k))

(defn representable-key? [qualifier k]
  (not (and qualifier (ident? k) (= "_" (namespace k)))))

(defn key-loc [zloc]
  (when (and zloc (significant? (z/node zloc)))
    zloc))

(defn first-key [map-zloc]
  (key-loc (some-> map-zloc z/down skip-right)))

(defn next-key [zloc]
  (let [v (-> zloc z/right skip-right)]
    (when-not (z/rightmost? v)
      (key-loc (-> v z/right skip-right)))))

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

(defn expand-namespaced-map [zloc]
  (let [node (z/node zloc)
        {:keys [row col]} (meta node)
        shift (- col (:col (meta (z/node (z/up zloc)))))
        dedent-by (if (= row (some-> (first-key zloc) z/node meta :row)) shift 0)
        children (vec (:children node))
        children (loop [i 0
                        col (inc (- col shift))
                        out []]
                   (if (= i (count children))
                     out
                     (let [child (children i)
                           line-start (and (pos? i)
                                           (#{:newline :comment} (node/tag (children (dec i)))))
                           k (when (:map-qualifier child) (node/sexpr child))
                           pos (meta child)
                           child (cond
                                   (and line-start (= :whitespace (node/tag child)))
                                   (let [width (- (count (node/string child)) dedent-by)]
                                     (when (pos? width) (node/spaces width)))
                                   (ident? k) (node/coerce k)
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

(defn mark-for-positional-recalc [node]
  (vary-meta node c/assoc :rewrite-edn/positional-recalc true))

(defn recalc-positional-metadata [node]
  (if (:rewrite-edn/positional-recalc (meta node))
    (-> node
        str
        p/parse-string-all)
    node))

(declare update)

(defn assoc [forms k v]
  (update forms k (constantly v) nil))

(defn lookup [node k]
  (let [zloc (enter-namespaced-map (z/of-node node))
        elems (significant-children zloc)]
    (case (z/tag zloc)
      :map (some (fn [[k' v]] (when (= k (node/sexpr k')) v)) (partition 2 elems))
      :set (some #(when (= k (node/sexpr %)) %) elems)
      :vector (when (and (integer? k) (< -1 k (count elems))) (nth elems k))
      nil)))

(defn get [node k default]
  (or (lookup node k) (node/coerce default)))

(defn get-in [node ks not-found]
  (or (reduce #(or (lookup %1 %2) (reduced nil)) node ks)
      (node/coerce not-found)))

(defn replace-with [zloc f args]
  (z/root (z/replace zloc (node/coerce (apply f (z/node zloc) args)))))

(defn update-vector [zloc k f args]
  (when-not (< -1 k (count (significant-children zloc)))
    (throw #?(:clj (java.lang.IndexOutOfBoundsException.)
              :cljs (ex-info "IndexOutOfBounds" {}))))
  (loop [i 0
         zloc (-> zloc z/down skip-right)]
    (if (= i k)
      (replace-with zloc f args)
      (recur (inc i) (-> zloc z/right skip-right)))))

(declare update*)

(defn update-map [zloc k f args]
  (let [qualifier (map-qualifier zloc)
        k-node (node/coerce (qualify-key qualifier k))
        new-v #(node/coerce (apply f (node/coerce nil) args))
        insert (fn [zloc key-count align-loc]
                 (-> zloc
                     (z/insert-right* k-node)
                     (indent-or-space key-count align-loc)
                     z/right
                     (z/insert-right (new-v))
                     z/root))]
    (if-not (representable-key? qualifier k)
      (update* (expand-namespaced-map zloc) k f args)
      (if-let [key-zloc (first-key zloc)]
        (loop [key-count 0
               zloc key-zloc]
          (if (= (z/sexpr zloc) k)
            (replace-with (-> zloc z/right skip-right) f args)
            (if-let [next-zloc (next-key zloc)]
              (recur (inc key-count) next-zloc)
              (insert (z/rightmost zloc) (inc key-count) (meta (z/node key-zloc))))))
        (if-let [comment-zloc (some-> zloc z/down* (z/find z/right* comment-loc?))]
          (insert (z/skip z/left* z/whitespace? (z/rightmost* comment-zloc))
                  0 (meta (z/node comment-zloc)))
          (-> zloc
              (z/append-child k-node)
              (z/append-child (new-v))
              z/root))))))

(defn update* [forms k f args]
  (let [zloc (find-target forms #{:token :map :vector :namespaced-map})
        zloc (cond-> zloc
               (nil-node? (z/node zloc)) (z/replace (node/coerce {})))]
    (if (= :vector (z/tag zloc))
      (update-vector zloc k f args)
      (update-map zloc k f args))))

(defn update
  ([forms k f]
   (update forms k f nil))
  ([forms k f args]
   (-> (recalc-positional-metadata forms)
       (update* k f args)
       mark-for-positional-recalc)))

(defn update-in [forms keys f args]
  (if (= 1 (count keys))
    (update forms (first keys) f args)
    (update forms (first keys) #(update-in % (rest keys) f args))))

(defn assoc-in [forms keys v]
  (update-in forms keys (constantly v) nil))

(declare keys)

(defn replace-keys [new-ks forms]
  (let [map-zloc (find-target forms #{:map :namespaced-map})
        qualifier (map-qualifier map-zloc)
        zloc (first-key map-zloc)
        expand? (some #(not (representable-key? qualifier %)) new-ks)]
    (cond
      expand? (replace-keys new-ks (expand-namespaced-map map-zloc))
      (nil? zloc) forms
      :else
      (loop [zloc zloc
             new-ks new-ks]
        (let [zloc (z/replace zloc (node/coerce (qualify-key qualifier (first new-ks))))]
          (if-let [next-zloc (next-key zloc)]
            (recur next-zloc (rest new-ks))
            (z/root zloc)))))))

(defn map-keys [f forms]
  (replace-keys (mapv (comp f node/sexpr) (reverse (keys forms))) forms))

(defn dissoc [forms k]
  (let [zloc (find-target forms #{:token :map :namespaced-map})]
    (loop [zloc (first-key zloc)]
      (cond
        (nil? zloc) forms
        (= (z/sexpr zloc) k)
        (let [key-node (z/node zloc)]
          (-> zloc z/right skip-right z/remove
              (z/find z/prev #(identical? key-node (z/node %)))
              z/remove z/root))
        :else (recur (next-key zloc))))))

(defn keys [forms]
  (into () (take-nth 2) (significant-children (find-target forms #{:map :namespaced-map}))))

(defn conj* [forms v]
  (let [zloc (z/of-node forms)]
    (if (nil-node? (z/node zloc))
      (z/root (z/replace zloc (node/coerce (list v))))
      (case (z/tag zloc)
        (:vector :set) (z/root (z/append-child zloc (node/coerce v)))
        :list (z/root (z/insert-child zloc (node/coerce v)))
        (:map :namespaced-map) (assoc forms (first v) (second v))
        (throw (ex-info "Unsupported forms" {:forms forms}))))))

(defn conj [forms v]
  (-> (recalc-positional-metadata forms)
      (conj* v)
      mark-for-positional-recalc))

(defn fnil [f nil-replacement]
  (fn [x & args]
    (if (nil-node? x)
      (apply f (node/coerce nil-replacement) args)
      (apply f x args))))
