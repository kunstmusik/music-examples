(ns music-examples.ugen-arrays
  (:require [pink.simple :refer :all] 
            [pink.oscillators :refer :all]
            [pink.config :refer :all]
            [pink.util :refer :all]
            [pink.space :refer :all]
            [hiphip.double :as dbl]
            [clojure.pprint :refer [pprint]]
            ))


(defn sine-amap
  "Sine generator with fixed frequency and starting phase"
    ([^double freq]
     (sine-amap freq 0.0))
    ([^double freq ^double phase]
     (let [phsr  (phasor freq phase)]
       (fn []
        (when-let [buf ^doubles (phsr)]
          (amap buf indx ret
               (Math/sin (* TWO_PI (aget buf indx)))) 
          )))))

(defn sine-hiphip 
  "Sine generator with fixed frequency and starting phase"
  ([^double freq]
   (sine-hiphip freq 0.0))
  ([^double freq ^double phase]
   (let [phsr  (phasor freq phase)]
       (fn []
        (when-let [buf ^doubles (phsr)]
          (dbl/amap [x buf]
            (Math/sin (* TWO_PI x)))
           )))))

;(pprint ((sine-amap 440.0)))
;(pprint ((sine-hiphip 440.0)))
;(pprint ((sine 440.0)))

(defn run-test
  [msg afn afn2 afn3]
  (println msg)
  (doseq [_ (range 3)]
    (time
      (doseq [_ (range 100000)]
        (afn)
        (afn2)
        (afn3)
        ))
    ))

(defn test-sine-amap []
  (run-test "[sine-amap]" 
            (sine-amap 440.0) 
            (sine-amap 550.0)
            (sine-amap 660.0)))

(defn test-sine-hiphip []
  (run-test "[sine-hiphip]" 
            (sine-hiphip 440.0)
            (sine-hiphip 550.0)
            (sine-hiphip 660.0)))

(defn test-sine []
  (run-test "[sine]" 
            (sine 440.0)
            (sine 550.0)
            (sine 660.0)))

(defn run-all-tests []
  (test-sine-amap) 
  (test-sine-hiphip) 
  (test-sine))

(comment
  (run-all-tests)
  

  (start-engine)

  (add-afunc (pan (mul 0.0025
                    (apply sum 
                           (for [x (range 120)]
                             (sine-amap (* x 60)))
                           )) 
                 0.0))

  (clear-engine)

  (add-afunc (pan (mul 0.0025
                    (apply sum 
                           (for [x (range 120)]
                             (sine-hiphip (* x 60)))
                           )) 
                 0.0))

  (clear-engine)

  (add-afunc (pan (mul 0.0025
                    (apply sum 
                           (for [x (range 120)]
                             (sine (* x 60)))
                           )) 
                 0.0))
  )
