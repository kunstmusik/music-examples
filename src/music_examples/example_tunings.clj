(ns music-examples.example-tunings
  (:require [clojure.java.io :refer [resource]]
            [clojure.pprint :refer [pprint]])
  (:require [score.core :refer :all]
            [score.bpf :refer :all]
            [score.amp :refer :all]
            [score.freq :refer :all]
            [score.tuning :refer :all]
            [score.util :refer :all])  
  (:require [pink.simple :refer :all]
            [pink.config :refer :all]
            [pink.envelopes :refer [env exp-env adsr xar]]
            [pink.oscillators :refer [oscili]]
            [pink.util :refer :all]
            [pink.instruments.horn :refer :all]
            [pink.space :refer :all]
            [pink.event :refer :all]))

(defn instr-horn
  [amp freq loc]
  (-> 
    (horn (mul amp (adsr 0.02 0.04 0.95 0.1)) freq) 
    (pan loc)))

(start-engine)

(comment

  ;; Adding simple events: Errors in that it does not add to engine
  (add-events
    (event instr-horn 0.0 0.25 440 0.0)
    (event instr-horn 1.0 0.25 660 0.0))

  ;; Adding simple events: Success, but instr-horn initialized in user thread 
  (add-events
    (event add-afunc 0.0 (instr-horn 0.25 440 0.0))
    (event add-afunc 1.0 (instr-horn 0.25 660 0.0)))

  ;; Adding simple events: Success, as events are wrapped as audio-events 
  (add-audio-events
    (event instr-horn 0.0 0.25 440 0.0)
    (event instr-horn 1.0 0.25 660 0.0))

  ;; Using Csound-style events: audio-func, start, duration, & args
  (add-audio-events 
    (i instr-horn 0.0 2.0 0.25 440 0.0)
    (i instr-horn 1.0 2.0 0.25 660 0.0))

  ;; Using with-afunc: separates data from instrument 
  (add-audio-events 
    (with-afunc instr-horn
      [0.0 2.0 0.25 440 0.0]
      [1.0 2.0 0.25 660 0.0]))

  ;; Using Score to use PCH notation 
  (add-audio-events 
    (with-afunc instr-horn
      (map (fn [n] 
             (swapv! n 3 #(pch->freq TWELVE-TET %)))
           [[0.0 2.0 0.25 [8 9] 0.0]
            [1.0 2.0 0.25 [9 4] 0.0]])))

  ;; Using Score to use keyword notation 
  (add-audio-events 
    (with-afunc instr-horn
      (map (fn [n] 
             (swapv! n 3 keyword->freq))
           [[0.0 2.0 0.25 :A4 0.0]
            [1.0 2.0 0.25 :E5 0.0]])))

  ;; Using process-notes macro to ease use of swapv! 
  (add-audio-events 
    (with-afunc instr-horn
      (let [notes 
            [[0.0 2.0 -6 :A3 0.0]
             [0.0 2.0 -8 :C#4 0.0]
             [0.0 2.0 -16 :E4 0.0]
             ]]
        (process-notes notes
                       2 db->amp 
                       3 keyword->freq))))

  ;; Using Score to generate notes, then wrap with instr-horn into 
  ;; audio events
  (add-audio-events
    (with-afunc instr-horn
      (gen-notes 
        (range 0 10 0.5)
        (cycle (shuffle [0.25 0.5 0.75])) 
        0.25
        (map #(pch->freq TWELVE-TET [8 %]) (range 0 20))
        0.0
        )))
  
  ;; alternate tuning
  (def gamma-scale 
    (create-tuning-from-file 
      (resource "carlos_gamma.scl")))

  (defn mirror [s]
    (into s (rest (reverse (rest s)))))

  (defn score-gamma-chord []
    (with-afunc instr-horn 
      (gen-notes
        (range 0 8 0.25)
        0.2
        0.2
        (map #(pch->freq gamma-scale [9 %]) 
             (cycle (mirror [5 11 17])))
        0.0
        )))

  (defn score-gamma2 []
    (with-afunc instr-horn
      (gen-notes
        (map #(+ 4 %) (range 0 4 0.125)) 
        0.1
        0.2
        (map #(pch->freq gamma-scale [8 %]) 
             (cycle (mirror [0 11 19 26])))
        0.0
        )))

  (add-audio-events (score-gamma-chord))
  (add-audio-events (score-gamma2))

  )



(comment

  (start-engine)

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
    (gen-notes
      (repeat instr-horn) 
      (range 0 16 0.25)
      0.25
      (map #(pch->freq gamma-scale [8 %]) (cycle [0 7 13 18 22]))
      0.0
      )
    )

  (schedule score-12TET)
  (schedule score-gamma)
  (schedule (score-gamma-chord))

  (stop-engine)

  )
