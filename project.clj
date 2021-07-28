(defproject openapi-spec-provider "0.0.1-SNAPSHOT"
  :description "Spec Provider for OpenAPI schemas."
  :url "https://github.com/alvinfrancis/openapi-spec-provider"
  :license {:name         "Eclipse Public License"
            :url          "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :source-paths ["src/clj"]
  :plugins [[lein-codox "0.10.7"]]
  :dependencies [[clj-commons/clj-yaml "0.7.106"]
                 [camel-snake-kebab "0.4.2"]
                 ;[org.clojure/tools.analyzer "1.0.0"]
                 ;[org.clojure/tools.analyzer.jvm "1.1.0"]
                 ]
  :codox {:output-path "codox"
          :metadata    {:doc/format :markdown}
          :source-uri  "https://github.com/alvinfrancis/opentracing-clj/blob/v{version}/{filepath}#L{line}"}
  :deploy-repositories [["snapshots" {:url      "https://clojars.org/repo"
                                      :username [:env/clojars_username :gpg]
                                      :password [:env/clojars_password :gpg]}]
                        ["releases"  {:url           "https://clojars.org/repo"
                                      :username      [:env/clojars_username :gpg]
                                      :password      [:env/clojars_password :gpg]
                                      :sign-releases false}]]
  :profiles {:provided {:dependencies [[org.clojure/clojure "1.10.2"]]}})
