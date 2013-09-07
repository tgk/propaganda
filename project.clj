(defproject propaganda "0.0.3-SNAPSHOT"
  :description "(Hopefully) extensible propagator library"
  :url "http://github.com/tgk/propaganda"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]

                 ;; convenient for debugging
                 [dire "0.4.4"]

                 ]
  :plugins [[lein-release "1.0.4"]]
  :lein-release {:scm :git
                 :deploy-via :shell
                 :shell ["lein" "deploy" "clojars"]})
