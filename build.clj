(ns build
  (:require
   [clojure.string :as str]
   [clojure.tools.build.api :as b]
   [utils]))

(def lib 'borkdude/rewrite-edn)
(def version (utils/format-version))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [opts]
  (when (:verbose opts)
    (println "Building jar..."))
  (clean nil)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs ["src"]
                :scm {:url "https://github.com/borkdude/rewrite-edn"
                      :connection "scm:git:git@github.com:borkdude/rewrite-edn.git"
                      :developerConnection "scm:git:git@github.com:borkdude/rewrite-edn.git"
                      :tag (format "v%s" version)}
                :pom-data
                [[:description "Rewrite EDN"]
                 [:url "https://github.com/borkdude/rewrite-edn"]
                 [:licenses
                  [:license
                   [:name "Eclipse Public License 1.0"]
                   [:url "http://opensource.org/licenses/eclipse-1.0.php"]]]]})
  (b/copy-dir {:src-dirs ["src"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

(defn install [_]
  (jar {})
  (b/install {:basis basis
              :lib lib
              :version version
              :jar-file jar-file
              :class-dir class-dir}))

(defn deploy [opts]
  (jar opts)
  (try ((requiring-resolve 'babashka.deps-deploy/deploy)
        (merge {:installer :remote
                :artifact jar-file
                :pom-file (b/pom-path {:lib lib :class-dir class-dir})}
               opts))
       (catch Exception e
         (if-not (str/includes? (ex-message e) "redeploying non-snapshots is not allowed")
           (throw e)
           (println "This release was already deployed."))))
  opts)
