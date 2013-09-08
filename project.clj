(defproject propaganda "0.0.5-SNAPSHOT"
  :description "(Hopefully) extensible propagator library"
  :url "http://github.com/tgk/propaganda"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :plugins [[lein-release "1.0.4"]
            [lein-cljsbuild "0.3.2"]]
  :hooks [leiningen.cljsbuild]
  :source-paths ["src"]
  :lein-release {:scm :git
                 :deploy-via :shell
                 :shell ["lein" "deploy" "clojars"]}
  :clojurescript? true
  :cljsbuild
  {:builds {:main
            {:source-paths ["src-cljs"]
             :jar true
             :compiler {:output-to "js/propaganda.js"
                        :optimizations :advanced
                        :pretty-print false}}
            :dev
            {:source-paths ["src-cljs"]
             :compiler {:optimizations :whitespace
                        :pretty-print true}}}
   :crossovers [propaganda.values
                propaganda.system
                propaganda.generic-operators
                propaganda.support-values
                propaganda.intervals.common
                propaganda.intervals.system]
   :crossover-jar true
   :crossover-path "crossover"})
