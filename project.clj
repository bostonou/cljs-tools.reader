(defproject tools.reader "0.1.0-SNAPSHOT"
  :description "tools.reader ported to cljs"
  :url "https://github.com/bostonou/cljs-tools.reader"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0-beta2"]
                 [org.clojure/clojurescript "0.0-3211"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [figwheel "0.2.9"]]

  :plugins [[lein-cljsbuild "1.0.5"]
            [lein-figwheel "0.2.9"]]

  :source-paths ["src"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]
  
  :cljsbuild
  {:test-commands {"test" ["node" :runner "resources/test/tools.reader.test.js"]}

   :builds
   [{:id "test"
     :source-paths ["src/main" "src/test"]
     :notify-command ["node" "resources/test/tools.reader.test.js"]
     :compiler
     {:output-to  "resources/test/tools.reader.test.js"
      :source-map "resources/test/tools.reader.test.js.map"
      :output-dir "resources/test/out"
      :optimizations :simple}}
    ]}

  :figwheel {
             ;; :http-server-root "public" ;; default and assumes "resources" 
             ;; :server-port 3449 ;; default
             :css-dirs ["resources/public/css"] ;; watch and update CSS

             ;; Start an nREPL server into the running figwheel process
             ;; :nrepl-port 7888

             ;; Server Ring Handler (optional)
             ;; if you want to embed a ring handler into the figwheel http-kit
             ;; server, this is for simple ring servers, if this
             ;; doesn't work for you just run your own server :)
             ;; :ring-handler hello_world.server/handler

             ;; To be able to open files in your editor from the heads up display
             ;; you will need to put a script on your path.
             ;; that script will have to take a file path and a line number
             ;; ie. in  ~/bin/myfile-opener
             ;; #! /bin/sh
             ;; emacsclient -n +$2 $1
             ;;
             ;; :open-file-command "myfile-opener"

             ;; if you want to disable the REPL
             ;; :repl false

             ;; to configure a different figwheel logfile path
             ;; :server-logfile "tmp/logs/figwheel-logfile.log" 
             })
