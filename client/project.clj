(defproject splendor "0.1.0-SNAPSHOT"
  :description "Splendor front-end"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.9.0-alpha11"]
                 [org.clojure/clojurescript "1.9.225"]
                 [reagent "0.6.0-rc"]]

  :plugins [[lein-cljsbuild "1.1.4"]]

  :source-paths ["src"]

  :cljsbuild
  {:builds [{:id "splendor"
             :source-paths ["src"]
             :compiler
             {:output-to "splendor.js"
              :output-dir "out"
              :optimizations :none
              :source-map true}}]})
