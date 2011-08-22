(defproject repl "0.0.1"
  :description "Kevin's Clojure REPL for small jobs"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
		 [dcm4che/dcm4che-core "2.0.24"]
		 [org.nrg/log4jr-client "0.1.0"]
		 [log4j/log4j "1.2.16"]
		 [org.slf4j/slf4j-api "1.6.1"]
		 [org.slf4j/slf4j-log4j12 "1.6.1"]
		 [compojure "0.6.4"]
		 [hiccup "0.3.6"]]
  :dev-dependencies [[ring-serve "0.1.1"]])
