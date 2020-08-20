(defproject mrcsce/decision-tree "0.1.0"
  :description "Clojure's implementation of decision-tree.
   see also https://github.com/Miyoshi-Ryota/decision-tree-clojure"
  :url "https://github.com/Miyoshi-Ryota/decision-tree-clojure"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/mit-license.php"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :repositories [["clojars" {:url "https://clojars.org/repo" :sign-releases false}]]
  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]]
  :repl-options {:init-ns decision-tree.core})
