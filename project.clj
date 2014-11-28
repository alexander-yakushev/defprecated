(defproject defprecated "0.1.0"
  :description "Deprecation in Clojure made easy"
  :url "https://github.com/alexander-yakushev/defprecated"
  :license {:name "CC0"
            :url "http://creativecommons.org/about/cc0"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :profiles {:dev {:dependencies [[midje "1.6.3"]]
                   :plugins [[lein-midje "3.1.3"]]}})
