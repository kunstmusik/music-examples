(ns music-examples.example2
  (:require 
    [score.core :refer :all]
    [score.bpf :refer :all]
    [score.freq :refer :all])  
  (:require [pink.audio.engine :as eng]
            [pink.audio.envelopes :refer [env exp-env adsr xadsr xar]]
            [pink.audio.oscillators :refer [oscili]]
            [pink.audio.util :refer [mul sum let-s reader]]
            [pink.event :refer :all]))


(defn table-synth [amp freq dur]
  (println "Truncating...")
  (mul
     (oscili amp freq)
     (env [0.0 0.0, 0.05 1, 0.02 0.5, dur 0.5, 0.2 0])))

(defn score->events
  [score]
  (map #(apply event %) score))

(defn schedule 
  [e notes]
  (engine-events e 
    (score->events notes)))

(comment

  (defn my-score [e] 
    (schedule
      (gen-notes
        (repeat table-synth)
        0.0 
        0.25
        (map #(env [0.0 (* 440.0 %) 5.0 (* 880 %)])
             (range 1 3 0.5))
        5.0
        )))
    

  (let [e (eng/engine-create)
        simple-glissandi (my-score e) 
        ]
    
      (eng/engine-start e)
      (eng/engine-add-afunc e (eng-events-runner simple-glissandi))

      (Thread/sleep 6000)
      (eng/engine-stop e)
      (eng/engine-clear e)))



