(ns borkdude.rewrite-edn-test
  (:require [borkdude.rewrite-edn :as r]
            [clojure.test :as t :refer [deftest is testing]]
            [rewrite-cljc.parser :as p]))

(deftest assoc-test
  (is (= "{:a 1 :b 1}" (str (r/assoc
                             (p/parse-string "{:a 1}")
                             :b 1))))
  (is (= "{:a #_:something 2}" (str (r/assoc
                                     (p/parse-string "{:a #_:something 1}")
                                     :a 2)))))
