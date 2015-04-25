(defproject kindness "0.1.0-SNAPSHOT"
  :description "Ludum Dare 32 Entry: An Unconventional Weapon"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2850"]
                 [figwheel "0.2.5-SNAPSHOT"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [overtone "0.9.1"]
                 [leipzig "0.8.1"]
                 [environ "1.0.0"]
                 [amazonica "0.3.21"]]
  :node-dependencies [[source-map-support "0.2.8"]]

  :plugins [[lein-cljsbuild "1.0.4"]
            [lein-npm "0.4.0"]
            [lein-figwheel "0.2.5-SNAPSHOT"]]

  :source-paths ["src/clj" "src-dev/clj" "target/classes"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled"]

  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src/cljs" "src-dev/cljs"]
                        :compiler {
                                   :main kindness.dev
                                   :output-to "resources/public/js/compiled/out/kindness.js"
                                   :output-dir "resources/public/js/compiled/out"
                                   :optimizations :none
                                   :cache-analysis true
                                   :source-map true
                                   :asset-path "js/compiled/out"}}
                       {:id "release"
                        :source-paths ["src/cljs"]
                        :compiler {
                                   :main kindness.game
                                   :output-to "resources/public/js/compiled/out-adv/kindness.min.js"
                                   :output-dir "resources/public/js/compiled/out-adv"
                                   :optimizations :advanced
                                   :pretty-print false}}]}

  :figwheel {:http-server-root "public"})
