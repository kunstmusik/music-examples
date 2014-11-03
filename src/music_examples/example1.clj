(ns music-examples.example1
  (:require 
    [score.core :refer :all]
    [score.bpf :refer :all]
    [score.freq :refer :all])  
  (:require [pink.simple :refer :all]
            [pink.envelopes :refer :all]
            [pink.oscillators :refer :all]
            [pink.space :refer :all]
            [pink.util :refer :all]
            [pink.event :refer :all]))

;; Example 1 - Basic Engine Use - Add/Remove Audio Functions

(comment

  ; Start Default Pink Audio Engine
  (start-engine)  

  ; Create Sine Audio Function
  (def s (pan (sine 440.0)
              0.0))

  ; Add Sine s to Default Audio Engine
  (add-afunc s)

  ; Remove Sine s from Default Audio Engine
  (remove-afunc s)

  ; Stop Default Pink Audio Engine
  (stop-engine)
  
  )

(defn fm-bell 
  "Simple frequency-modulation bell"
  [freq]
  (let-s [e (xar 0.0001 1.3)] 
    (mul
        (sine2 (sum freq (mul freq (sine (* 4.77 freq)))))
        (mul 0.05 e))))

(defn score->events
  "Utility function for creating events from Score note lists"
  [score]
  (map #(apply event %) score))

(comment
 
  (let [events 
        (score->events 
          (gen-notes2 0.0 5.0
                      (repeat fm-bell)
                      (repeat 0.5)
                      (bpf [0 440 5.0 880])))]
    
      (start-engine)
      (add-audio-events events)
      (add-events (event stop-engine 6.0))
      ;(Thread/sleep 6000)
      ;(stop-engine)
      ;(clear-engine)
      
      )

  )
