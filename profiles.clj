{:user {:dependencies [[acyclic/squiggly-clojure "0.1.2-SNAPSHOT"]
                       [cheshire "5.4.0"]
                       [expectations "2.0.16"]
                       [org.clojars.gjahad/debug-repl "0.3.3"]
                       [org.clojure/tools.nrepl "0.2.7"]
                       #_[quil "2.2.5"]
                       #_[slamhound "1.5.5"]]

        :injections [(use '[clojure.pprint :only [pp pprint]])
                     (use 'alex-and-georges.debug-repl)]

        :plugins [#_[alembic "0.3.2"]
                  #_[cc.artifice/lein-gossip "0.2.1"]
                  [cider/cider-nrepl "0.8.2"]
                  [cloc "0.1.0"]
                  [codox "0.8.11"]
                  [gargamel "0.5.0"]
                  [jonase/eastwood "0.2.1"]
                  [lein-ancient "0.6.5"]
                  [lein-bikeshed "0.2.0"]
                  #_[lein-clique "0.1.2" :exclusions [org.clojure/clojure]]
                  [lein-exec "0.3.4"]
                  #_[lein-fruit "0.2.2"]
                  [lein-hiera "0.9.0"]
                  [lein-instant-cheatsheet "2.1.4"]
                  [lein-kibit "0.0.8"]
                  [lein-marginalia "0.8.0"]
                  #_[lein-ns-dep-graph "0.1.0-SNAPSHOT"]
                  #_[lein-plantuml "0.1.14"]
                  [lein-plz "0.3.3" :exclusions [[rewrite-clj] [ancient-clj]]]
                  [lein-typed "0.3.5"]
                  #_[org.clojure/core.async "0.1.346.0-17112a-alpha"]
                  [org.timmc/nephila "0.3.0"]]

        :aliases {"slamhound" ["run" "-m" "slam.hound"]}

        :datomic
        {:install-location
         "/Users/yurrriq/.m2/repository/com/datomic/datomic-pro/0.9.5153"}}}
