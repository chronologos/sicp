(defproject sicp "0.1.0"
  :description "Working through the book 'Structure and Interpretation of Computer Programs' in Clojure."
  :url "https://www.iantay.dev/tags/sicp/"
  :license {:name "EPL-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/tools.trace "0.7.11"]
                 [quil "3.1.0"]]
  :main  sicp.ch2quil
  :repl-options {:init-ns sicp.util})
