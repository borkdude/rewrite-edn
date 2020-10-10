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
               :a inc)))))

(deftest map-keys-test
  (is (= "
{foo/foo 1
 bar/bar 2}"
         (str (r/map-keys
               (fn [sym]
                 (if (qualified-symbol? sym)
                   sym
                   (symbol (str sym) (str sym))))
               (r/parse-string "
{foo 1
 bar 2}"))))))

(defn qualify-sym [sym]
  (if (qualified-symbol? sym) sym
      (symbol (str sym) (str sym))))

(deftest update-deps-test
  (is (= "
{:deps {foo/foo {:mvn/version \"0.1.0\"}}}"
         (str (r/update (r/parse-string "{:deps {foo {:mvn/version \"0.1.0\"}}}")
                        :deps
                        (fn [deps-node]
                          (r/map-keys qualify-sym deps-node)))))))
