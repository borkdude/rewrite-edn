(ns borkdude.rewrite-edn-test
  (:require [borkdude.rewrite-edn :as r]
            [clojure.test :as t :refer [deftest is]]))

(deftest assoc-test
  (is (= "{:a 1 :b 1}"
         (str (r/assoc
               (r/parse-string "{:a 1}")
               :b 1))))
  (is (= "{:a 2}"
         (str (r/assoc
               (r/parse-string "{:a 1}")
               :a 2))))
  (is (= "{:a #_:something 2}"
         (str (r/assoc
               (r/parse-string "{:a #_:something 1}")
               :a 2))))
  (is (= "{:a 2} ;; this is a cool map"
         (str (r/assoc
               (r/parse-string "{:a 1} ;; this is a cool map")
               :a 2)))))

(deftest update-test
  (is (= "{:a #_:foo 2}"
         (str (r/update
               (r/parse-string "{:a #_:foo 1}")
               :a (fn [node]
                    (inc (r/sexpr node))))))))

(defn qualify-sym-node [sym-node]
  (let [sym (r/sexpr sym-node)]
    (if (or (not (symbol? sym))
            (qualified-symbol? sym))
      sym-node
      (symbol (str sym) (str sym)))))

(deftest map-keys-test
  (is (= "
{foo/foo 1
 bar/bar 2}"
         (str (r/map-keys qualify-sym-node
                          (r/parse-string "
{foo 1
 bar 2}"))))))

(deftest update-deps-test
  (is (= "{:deps {foo/foo {:mvn/version \"0.1.0\"}}}"
         (str (r/update (r/parse-string "{:deps {foo {:mvn/version \"0.1.0\"}}}")
                        :deps
                        (fn [deps-map-node]
                          (r/map-keys qualify-sym-node deps-map-node)))))))

(deftest update-in-test
  (is (= "{:deps {foo/foo {:mvn/version \"0.2.0\"}}}"
         (str (r/update-in (r/parse-string "{:deps {foo/foo {:mvn/version \"0.1.0\"}}}")
                        [:deps 'foo/foo]
                        #(r/assoc % :mvn/version "0.2.0"))))))
