(ns music-examples.example-tunings
  (:require [clojure.java.io :refer [resource]])
  (:require [score.core :refer :all]
            [score.bpf :refer :all]
            [score.freq :refer :all]
            [score.tuning :refer :all])  
  (:require [pink.engine :refer :all]
            [pink.config :refer :all]
            [pink.envelopes :refer [env exp-env adsr xadsr xar]]
            [pink.oscillators :refer [oscili]]
            [pink.util :refer [mul sum let-s reader const create-buffer fill]]
            [pink.instruments.horn :refer :all]
            [pink.space :refer :all]
            [pink.event :refer :all]))


(defn instr-horn
  [amp freq loc]
  (pan 
    (horn amp freq) loc))

(defn schedule 
  [e notes]
  (->> (map #(apply event %) notes)
       (engine-events e)
       (engine-add-events e)))

(comment

  (def e (engine-create :nchnls 2))  
  (engine-start e)

  (def score-12TET
    (gen-notes 
      (repeat instr-horn)
      (range 0 10 0.5)
      0.25
      (map #(pch->freq TWELVE-TET [8 %]) (range 0 20))
      0.0
      ))

  (def gamma-scale 
    (create-tuning-from-file 
      (resource "carlos_gamma.scl")))

  (def score-gamma
    (gen-notes 
      (repeat instr-horn)
      (range 0 20 0.5)
      0.25
      (map #(pch->freq gamma-scale [8 %]) (range 0 35))
      0.0
      ))

  (def chord1 [0 4 9 15])
  (def chord2 [0 3 7 12])
  (def chord3 [0 4 9 17])

  (defn score-gamma-chord []
    (letfn [(env1 [] 
              (env [0 0 0.01 0.2 0.05 0.15 
                    0.2 0.15 0.05 0.0]))  
            (env2 [] 
              (env [0 0 0.01 0.2 0.05 0.15 
                    0.1 0.15 0.05 0.0]))]
      (mapcat 
        (fn [start env chord] 
          (gen-notes 
            (repeat instr-horn)
            start 
            (repeatedly env) 
            (map #(pch->freq gamma-scale [8 %]) chord)
            0.0
            ))
        (range 0 100 0.25) 
        (cycle [env1 env2])
        (concat (repeat 8 chord1)
                (repeat 8 chord2)
                (repeat 8 chord3)
                (repeat 8 chord1))
        )))

  (schedule e score-12TET)
  (schedule e score-gamma)
  (schedule e (score-gamma-chord))

  (engine-stop e)
  (engine-clear e)
  (engine-kill-all)  

  )
