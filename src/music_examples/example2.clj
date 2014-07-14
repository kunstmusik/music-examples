(ns music-examples.example2
  (:require 
    [score.core :refer :all]
    [score.bpf :refer :all]
    [score.freq :refer :all])  
  (:require [pink.audio.engine :as eng]
            [pink.audio.envelopes :refer [env exp-env adsr xadsr xar]]
            [pink.audio.oscillators :refer [oscili]]
            [pink.audio.util :refer [mul sum let-s reader const create-buffer fill]]
            [pink.event :refer :all]))


(defn table-synth [amp freq dur]
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


(defn white-noise []
  (let [out (create-buffer)] 
    (fn []
      (fill out  (double-array 1 0)
            (fn [a] (- (* 2 (Math/random) 1)))) 
      ))
  )

(defn my-score [e] 
  (schedule e
            (gen-notes
              (repeat table-synth)
              0.0 
              0.05
              (map #(mul (const (* % 440.0)) 
                         (sum 2.0 (mul (white-noise) 
                                       (env [0.0 0.5 1.0 1.0 2.0 1.0 5.0 0.0]) 
                                       )))
                   (range 1 3 0.5))
              5.0
              )))

(comment

  (def e (eng/engine-create))  
  (eng/engine-start e)

  (eng/engine-add-afunc e (eng-events-runner (my-score e)))

  (binding [eng/*ksmps* 256]
      (eng/engine-add-afunc e (eng-events-runner (my-score e)))
    )

  (eng/engine-stop e)
  (eng/engine-clear e)  

  (let [e (eng/engine-create)
        simple-glissandi (my-score e) 
        ]
    
      (eng/engine-start e)
      (eng/engine-add-afunc e (eng-events-runner simple-glissandi))

      (Thread/sleep 6000)
      (eng/engine-stop e)
      (eng/engine-clear e)))



