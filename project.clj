(defproject music-examples "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts 
  ^:replace
  ["-server"
   "-Xmx512m"           ; Minimum and maximum sizes of the heap
   "-XX:+UseG1GC"
   "-XX:MaxGCPauseMillis=1"     ; Specify a target of 20ms for max gc pauses
   ;"-XX:MaxNewSize=257m"         ; Specify the max and min size of the new
   ;"-XX:NewSize=256m"            ;  generation to be small
   "-XX:+UseTLAB"                ; Uses thread-local object allocation blocks. This
   ;  improves concurrency by reducing contention on
   ;  the shared heap lock.
   ;"-XX:MaxTenuringThreshold=0" ; Makes the full NewSize available to every NewGC 

   ; for GC diagnostics 
   ;"-XX:+PrintGCDetails"
   ;"-XX:+PrintGCTimeStamps"
   ]

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [kunstmusik/pink "0.4.1"]
                 [kunstmusik/score "0.4.0"]
                 [incanter "1.5.4"]
                 [prismatic/hiphip "0.2.0"]
                 [de.sciss/netutil "1.0.1"]
                 ]
  :profiles  {
              :dev  {
                     :global-vars  {*warn-on-reflection* true}
                     }

              :profiling  {
                           :plugins  [[lein-nodisassemble "0.1.3"]] 
                           :global-vars  {*warn-on-reflection* true
                                          *unchecked-math* :warn-on-boxed
                                          }      

                           }}

  )
