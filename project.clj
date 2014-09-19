(defproject sparse "0.1.0"
  :description "Generate sparse bit array representations of fundamental data types."
  :url "http://github.com/mdaley/sparse"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.6.0"]]

  :lein-release {:deploy-via :clojars}

  :profiles {:dev {:plugins [[lein-release "1.0.5"]]}})
