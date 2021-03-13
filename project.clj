(defproject borkdude/rewrite-edn "0.0.1"
  :description "Rewrite EDN"
  :url "https://github.com/borkdude/rewrite-edn"
  :scm {:name "git"
        :url "https://github.com/borkdude/rewrite-edn"}
  :license {:name "Eclipse Public License 1.0"
            :url "http://opensource.org/licenses/eclipse-1.0.php"}
  :source-paths ["src"]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [rewrite-clj "1.0.572-alpha"]]
  ;; :plugins [[lein-codox "0.10.7"]]
  ;; :codox {:output-path "gh-pages"}
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_user
                                    :password :env/clojars_pass
                                    :sign-releases false}]])

