(ns borkdude.rewrite-edn-test
  (:require [borkdude.rewrite-edn :as r]
            [clojure.test :as t :refer [deftest testing is]]))

(deftest assoc-test
  (testing "Base case"
    (is (= "{:a 1}"
           (str (r/assoc
                 (r/parse-string "{}")
                 :a 1)))))
  (testing "When there's only one existing, keys are added on a new line"
    (is (= "
{:a 1
 :b 1}"
           (str (r/assoc
                 (r/parse-string "
{:a 1}")
                 :b 1)))))
  (testing "Unless there are already keys on the same line"
    (is (= "{:a 1 :b 2 :c 3}"
           (str (r/assoc
                 (r/parse-string "{:a 1 :b 2}")
                 :c 3)))))
  (testing "when map is already multi-line, new keys are added on new line"
    (is (= "
{:a 1
 :b 2}
;; this is a cool map"
           (str (r/assoc
                 (r/parse-string "
{:a 1}
;; this is a cool map")
                 :b 2)))))
  (testing "Updating existing val"
    (is (= "{:a 2}"
           (str (r/assoc
                 (r/parse-string "{:a 1}")
                 :a 2)))))
  (testing "Something between key and val"
    (is (= "{:a #_:something 2}"
           (str (r/assoc
                 (r/parse-string "{:a #_:something 1}")
                 :a 2)))))
  (testing "Comment at the end"
    (is (= "{:a 2} ;; this is a cool map"
           (str (r/assoc
                 (r/parse-string "{:a 1} ;; this is a cool map")
                 :a 2)))))
  (testing "Vector index assoc"
    (is (= "[9 8 99 7] ;; this is a cool vector"
           (str (r/assoc
                 (r/parse-string "[9 8 3 7] ;; this is a cool vector")
                 2 99)))))
  (testing "Vector last index assoc"
    (is (= "[9 8 3 99] ;; this is a cool vector"
           (str (r/assoc
                 (r/parse-string "[9 8 3 7] ;; this is a cool vector")
                 3 99)))))
  (testing "Vector assoc out of bounds"
    (is (try
          (r/assoc (r/parse-string "[9 8 3 7] ;; this is a cool vector") 9 99)
          false
          (catch java.lang.IndexOutOfBoundsException _ true))))
  (testing "Vector assoc out of bounds with ignored"
    (is (try
          (r/assoc (r/parse-string "[9 8 3 #_99 #_213 7] ;; this is a cool vector") 4 99)
          false
          (catch java.lang.IndexOutOfBoundsException _ true))))
  (testing "Repeated assoc"
    (is (= (str "{:a 2\n"
                " :b 3}")
           (-> "{}"
               r/parse-string
               (r/assoc :a 2)
               (r/assoc :b 3)
               str)))))

(deftest update-test
  (is (= "{:a #_:foo 2}"
         (str (r/update
               (r/parse-string "{:a #_:foo 1}")
               :a (fn [node]
                    (inc (r/sexpr node)))))))
  ;; unlike assoc, update does not currently indent a new item
  (is (= "{:a 0 :b 1}"
         (-> "{}"
             r/parse-string
             (r/update :a (constantly 0))
             (r/update :b (constantly 1))
             str))))

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

(deftest assoc-in-test
  (is (= "{:a {:b {:c 2}}}"
         (str (r/assoc-in (r/parse-string "{}")
                          [:a :b :c] 2))))
  (is (= "{:a {:b {:c 2}}}"
         (str (r/assoc-in (r/parse-string "nil")
                          [:a :b :c] 2))))
  (is (= "{:deps {foo/foo {:mvn/version \"0.2.0\"}}}"
         (str (r/assoc-in (r/parse-string "{:deps {foo/foo {:mvn/version \"0.1.0\"}}}")
                          [:deps 'foo/foo :mvn/version]
                          "0.2.0"))))
  (is (= "{:a 1 :b {:c 1}}"
         (str (r/assoc-in (r/parse-string "{:a 1}") [:b :c] 1))))
  (is (= (str "{:deps {foo {:mvn/version \"x\"}\n"
              "        bar {:mvn/version \"y\"}}}")
         (-> "{}"
             r/parse-string
             (r/assoc-in [:deps 'foo] {:mvn/version "x"})
             (r/assoc-in [:deps 'bar] {:mvn/version "y"})
             str))))

(deftest update-in-test
  (is (= "{:deps {foo/foo {:mvn/version \"0.2.0\"}}}"
         (str (r/update-in (r/parse-string "{:deps {foo/foo {:mvn/version \"0.1.0\"}}}")
                           [:deps 'foo/foo]
                           #(r/assoc % :mvn/version "0.2.0")))))
  (is (= "{:a {:b {:c 1}}}"
         (str (r/update-in (r/parse-string "{}")
                           [:a :b :c]
                           (comp (fnil inc 0) r/sexpr)))))
  (is (= "{:a {:b {:c 1}}}"
         (str (r/update-in (r/parse-string "nil")
                           [:a :b :c]
                           (comp (fnil inc 0) r/sexpr)))))
  ;; unlike assoc-in, update-in does not currently indent a new item
  (is (= "{:a {:b {:c 1 :x 1}}}"
         (-> "{}"
             r/parse-string
             (r/update-in [:a :b :c] (comp (fnil inc 0) r/sexpr))
             (r/update-in [:a :b :x] (comp (fnil inc 0) r/sexpr))
             str))))

(deftest dissoc-test
  (is (= "{}" (str (r/dissoc (r/parse-string "{:a 1}") :a))))
  (is (= "{:a 1}" (str (r/dissoc (r/parse-string "{:a 1 \n\n:b 2}") :b))))
  (is (= "{:a 1\n:c 3}" (str (r/dissoc (r/parse-string "{:a 1\n:b 2\n:c 3}") :b))))
  (is (= "{:deps {foo/bar {}}}" (str (r/update (r/parse-string "{:deps {foo/bar {} foo/baz {}}}")
                                               :deps #(r/dissoc % 'foo/baz))))))

(deftest get-test
  (is (= "999" (str (r/get (r/parse-string "{:foo/bar 999 :foo 123}") :foo/bar))))
  (is (= "123" (str (r/get (r/parse-string "{:foo/bar 999 :foo 123}") :foo))))
  (is (= "nil" (str (r/get (r/parse-string "{:foo/bar 999 :foo 123}") :bar/baz))))
  (is (= "nil" (str (r/get (r/parse-string "{:foo/bar 999 :foo 123}") :bar/baz nil))))
  (is (= ":default" (str (r/get (r/parse-string "{:foo/bar 999 :foo 123}") :bar :default))))

  (is (= "99" (str (r/get (r/parse-string "[10 99 100 15]") 1))))
  (is (= "nil" (str (r/get (r/parse-string "[10 99 100 15]") 10))))
  (is (= "nil" (str (r/get (r/parse-string "[10 99 100 15]") 10 nil))))
  (is (= ":default" (str (r/get (r/parse-string "[10 99 100 15]") 10 :default)))))

(deftest keys-test
  (is (= #{:foo/bar :foo 'baz 'foo/baz 1}
         (->> (r/parse-string "{:foo/bar 999 :foo 123 baz 42 foo/baz 23 1 0}")
              r/keys
              (map r/sexpr)
              (into #{})))))

(deftest get-in-test
  (is (= "999" (str (r/get-in (r/parse-string "{:foo/bar 999 :foo 123}")
                              [:foo/bar]))))
  (is (= "999" (str (r/get-in (r/parse-string "{:foo/bar {:foo 999}}")
                              [:foo/bar :foo]))))
  (is (= "nil" (str (r/get-in (r/parse-string "{:foo/bar {}}")
                              [:foo/bar :bar/baz]))))
  (is (= "nil" (str (r/get-in (r/parse-string "{:foo/bar {}}")
                              [:foo/bar :bar/baz :foo]))))
  (is (= "nil" (str (r/get-in (r/parse-string "{:foo/bar 999 :foo 123}")
                              [:bar/baz] nil))))
  (is (= ":default" (str (r/get-in (r/parse-string "{:foo/bar 999 :foo 123}")
                                   [:bar] :default))))
  (is (= "{:foo :bar}" (str (r/get-in (r/parse-string "{:foo :bar}")
                                      [] :default))))

  (is (= "99" (str (r/get-in (r/parse-string "[10 99 100 15]")
                             [1]))))
  (is (= "99" (str (r/get-in (r/parse-string "[10 [0 99] 100 15]")
                             [1 1]))))
  (is (= "nil" (str (r/get-in (r/parse-string "[10 99 100 15]") [10]))))
  (is (= "nil" (str (r/get-in (r/parse-string "[10 [99] 100 15]") [1 10]))))
  (is (= "nil" (str (r/get-in (r/parse-string "[10 99 100 15]") [10] nil))))
  (is (= ":default" (str (r/get-in (r/parse-string "[10 99 100 15]")
                                   [10] :default)))))

(deftest threaded-test
  ;; identation continues to work with a mix of threaded operations
  (is (= (str "{:a {:b 1\n"
              "     :c 2\n"
              "     :d 3\n"
              "     :e 4}\n"
              " :x 7\n"
              " :z 9}")
         (-> "{:a {:b 1}}"
             r/parse-string
             (r/assoc :w 6)
             (r/assoc :x 6)
             (r/assoc :y 8)
             (r/dissoc :y)
             (r/update :x #(-> % r/sexpr inc))
             (r/assoc :z 9)
             (r/assoc-in [:a :c] 2)
             (r/assoc-in [:a :d] 2)
             (r/dissoc :w)
             (r/update-in [:a :d] #(-> % r/sexpr inc))
             (r/assoc-in [:a :e] 4)
             str))))

(deftest conj-test
  (testing "Update vector"
    (is (= "; comment \n[1 2 3 4]"
           (-> "; comment \n[]"
               (r/parse-string)
               (r/conj 1)
               (r/conj 2)
               (r/conj 3)
               (r/conj 4)
               str))
        "always conj to end of vector"))
  (testing "Update list"
    (is (= "(4 1 2 3)"
           (-> "()"
               (r/parse-string)
               (r/conj 3)
               (r/conj 2)
               (r/conj 1)
               (r/conj 4)
               str))
        "always conj to begin of list"))
  (testing "Update set"
    (is (= "#{1 2 3 4}"
           (-> "#{1 2 3}"
               (r/parse-string)
               (r/conj 4)
               str))))
  (testing "Update nil"
    (is (= "(4)"
           (-> "nil"
               (r/parse-string)
               (r/conj 4)
               str))
        "produce new list")
    (is (= "{0 (1)}"
           (-> "{0 nil}"
               (r/parse-string)
               (r/update 0 #(r/conj % 1))
               str))
        "produce new list in nested structure"))
  (testing "Update map"
    (is (= "{3 4}"
           (-> "{}"
               (r/parse-string)
               (r/conj [3 4])
               str))
        "conj should behave like assoc")
    (is (= "{nil nil}"
           (-> "{}"
               (r/parse-string)
               (r/conj [])
               (r/conj [])
               str))))
  (testing "Update with formatting"
    (is (= (str "   {3 4\n"
                "    5 6\n"
                "    7 8\n"
                "   }")
           (-> (str
                "   {3 4\n"
                "    5 6\n"
                "   }")
               (r/parse-string)
               (r/conj [7 8])
               str))))
  (testing "Combine with update/update-in"
    (is (= (str "{:a [1 2 3 4]"
                " :b (2 1)"
                " :c #{1}}")
           (-> "{:a [1 2 3]}"
               (r/parse-string)
               (r/update :a #(r/conj % 4))
               (r/update :b #(r/conj % 1))
               (r/update-in [:b] #(r/conj % 2))
               (r/conj [:c #{1}])
               str))))
  (testing "Update unsupported forms"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Unsupported"
                          (-> ":k"
                              (r/parse-string)
                              (r/conj 1)))))
  (testing "Update with fnil conj"
    (is (= "{:a [1 2 3] :b [1]}"
           (-> "{:a [1 2 3]}"
               (r/parse-string)
               (r/update :b (r/fnil r/conj []) 1)
               str)))))
