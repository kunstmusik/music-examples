(defproject music-examples "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  ;:jvm-opts ^:replace  []
  :jvm-opts ;["-server" "-Xmx2g" "-XX:-UseParallelGC"]
  ^:replace
  ["-Xms512m" "-Xmx1g"           ; Minimum and maximum sizes of the heap
   "-XX:+UseParNewGC"            ; Use the new parallel GC in conjunction with
   "-XX:+UseConcMarkSweepGC"     ;  the concurrent garbage collector
   "-XX:+CMSConcurrentMTEnabled" ; Enable multi-threaded concurrent gc work (ParNewGC)
   "-XX:MaxGCPauseMillis=20"     ; Specify a target of 20ms for max gc pauses
   "-XX:+CMSIncrementalMode"     ; Do many small GC cycles to minimize pauses
   "-XX:MaxNewSize=257m"         ; Specify the max and min size of the new
   "-XX:NewSize=256m"            ;  generation to be small
   "-XX:+UseTLAB"                ; Uses thread-local object allocation blocks. This
   ;  improves concurrency by reducing contention on
   ;  the shared heap lock.
   "-XX:MaxTenuringThreshold=0"] ; Makes the full NewSize available to every NewGC

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [kunstmusik/pink "0.1.0-SNAPSHOT"]
                 [kunstmusik/score "0.1.0-SNAPSHOT"]
                 ])
