(defproject defprecated "0.1.2"
  :description "Deprecation in Clojure made easy"
  :url "https://github.com/alexander-yakushev/defprecated"
  :license {:name "CC0"
            :url "http://creativecommons.org/about/cc0"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :profiles {:dev {:dependencies [[midje "1.6.3"]
                                  [cloverage "1.0.4"]]
                   :plugins [[lein-midje "3.1.3"]
                             [jonase/eastwood "0.2.1"]]
                   :eastwood {:namespaces [:source-paths]}
                   :aliases {"coverage" ["run" "-m" "cloverage.coverage"]}}
             :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0-alpha5"]]}}
  :aliases {"test-all" ["do" ["check"] ["midje"]]})
