(ns music-examples.example1
  (:require 
    [score.core :refer :all]
    [score.bpf :refer :all]
    [score.freq :refer :all])  
  (:require [pink.audio.engine :as eng]
            [pink.audio.envelopes :refer [env exp-env adsr xadsr xar]]
            [pink.audio.oscillators2 :refer [sine sine2]]
            [pink.audio.util :refer [mul sum let-s reader]]
            [pink.event :refer :all]))


;; TODO - clean this up...
(def index (atom 1))
(def t (reader index))
(reset! index 3.25)
(defn fm-bell [freq]
  (
   ;let-s [e (exp-env [0.0 0.00001 0.05 1.0 3 0.000001])] 
   let-s [e (xar 0.0001 1.3)] 
    (mul
        (sine2 (sum freq (mul freq t (sine (* 4.77 freq)))))
        (mul 0.05 e))))

(defn score->events
  [score]
  (map #(apply event %) score))

(comment
 
  (let [e (eng/engine-create)
        ;eng-events 
        ;(engine-events e
        ;               (event fm-bell 0.0 440.0) 
        ;               (event fm-bell 0.5 550.0) 
        ;               (event fm-bell 1.0 660.0) 
        ;               (event fm-bell 1.5 880.0))
        eng-events
          (apply engine-events e
            (score->events 
              (gen-notes2 0.0 5.0
                          (const fm-bell)
                          (const 0.5)
                          (bpf [0 440 5.0 880])
                          ))
            
            )
        ]
    
      (eng/engine-start e)
      (eng/engine-add-afunc e (eng-events-runner eng-events))

      (Thread/sleep 5000)
      (eng/engine-stop e)
      (eng/engine-clear e))


  )
