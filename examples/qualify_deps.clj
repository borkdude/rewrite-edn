(require '[borkdude.rewrite-edn :as r]
         '[clojure.edn :as edn])

(def edn-string (slurp "deps.edn"))

(def nodes (r/parse-string edn-string))

(defn qualify-sym-node [sym-node]
  (let [sym (r/sexpr sym-node)]
    (if (or (not (symbol? sym))
            (qualified-symbol? sym))
      sym-node
      (symbol (str sym) (str sym)))))

(defn fully-qualify-deps [node path]
  (r/update-in node path #(r/map-keys qualify-sym-node %)))

(def edn (edn/read-string edn-string))
(def paths
  (concat
   (when (:deps edn)
     [[:deps]])
   (keep (fn [[k v]]
           (when (:extra-deps v)
             [:aliases k :extra-deps])) (:aliases edn))))

(def updated-nodes
  (reduce fully-qualify-deps nodes paths))

(spit "deps.edn" (str updated-nodes))
