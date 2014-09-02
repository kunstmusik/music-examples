(ns music-examples.example1
  (:require 
    [score.core :refer :all]
    [score.bpf :refer :all]
    [score.freq :refer :all])  
  (:require [pink.engine :refer :all]
            [pink.envelopes :refer [env exp-env adsr xadsr xar]]
            [pink.oscillators :refer [sine sine2]]
            [pink.util :refer [mul sum let-s reader]]
            [pink.event :refer :all]))

(defn fm-bell 
  "Simple frequency-modulation bell"
  [freq]
  (let-s [e (xar 0.0001 1.3)] 
    (mul
        (sine2 (sum freq (mul freq t (sine (* 4.77 freq)))))
        (mul 0.05 e))))

(defn score->events
  "Utility function for creating events from Score note lists"
  [score]
  (map #(apply event %) score))

(comment
 
  (let [e (engine-create)
        eng-events
          (engine-events e
            (score->events 
              (gen-notes2 0.0 5.0
                          (repeat fm-bell)
                          (repeat 0.5)
                          (bpf [0 440 5.0 880]))))]
    
      (engine-start e)
      (engine-add-events e eng-events)

      (Thread/sleep 5000)
      (engine-stop e)
      (engine-clear e))

  )
