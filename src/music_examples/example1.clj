(ns music-examples.example1
  (:require [pink.simple :refer :all]
            [pink.engine :refer :all]
            [pink.envelopes :refer :all]
            [pink.oscillators :refer :all]
            [pink.space :refer :all]
            [pink.util :refer :all]
            [pink.event :refer :all])
  (:import [clojure.lang IFn]))

;; Example 1 - Basic Engine Use - Add/Remove Audio Functions


(comment

  ; Create Pink Audio Engine
  (def eng (engine-create :nchnls 2))  

  ; Start Pink Audio Engine
  (engine-start eng)

  ; Create Panned, Sine Audio Function
  (def s (pan (sine 440.0)
              0.0))

  (instance? IFn s)

  ; Add Sine s to Audio Engine
  (engine-add-afunc eng s)

  ; Remove Sine s from Audio Engine
  (engine-remove-afunc eng s)

  ; Stop Pink Audio Engine
  (engine-stop eng)
  
  )

;; Using pink.simple convenience namespace 
(comment

  ; Start Default Pink Audio Engine
  (start-engine)  

  ; Create Panned, Sine Audio Function
  (def s (pan (sine 440.0)
              0.0))

  ; Add Sine s to Default Audio Engine
  (add-afunc s)

  ; Remove Sine s from Default Audio Engine
  (remove-afunc s)

  ; Stop Default Pink Audio Engine
  (stop-engine)
  
  )



