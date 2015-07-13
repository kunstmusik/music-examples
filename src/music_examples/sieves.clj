(ns music-examples.sieves
  (:require [score.core :refer :all]
            [score.freq :refer :all]
            [score.sieves :refer :all])  
  (:require [pink.simple :refer :all]
            [pink.engine :refer :all]
            [pink.config :refer :all]
            [pink.control :refer :all]
            [pink.filters :refer :all]
            [pink.envelopes :refer :all]
            [pink.util :refer :all]
            [pink.node :refer :all]
            [pink.oscillators :refer :all]
            [pink.space :refer :all]
            [pink.event :refer :all]
            [pink.effects.ringmod :refer :all]
            [pink.effects.reverb :refer :all]
            ))

;; Example of using sieves to generate harmonies

;; ====================== 
;; INSTRUMENT DEFINITIONS 
;; ======================
(defn fm
  "Simple frequency-modulation sound with default 1.77:1 cm ratio"
  ([freq amp]
   (fm freq amp 0.4 1.77))
  ([freq amp fm-index mod-mult]
  (let [mod-freq (mul freq mod-mult)]
    (let-s [e (adsr 0.02 2.0 0.0 0.0)] 
      (->
        (sine2 (sum freq (mul freq fm-index e 
                              (sine2 mod-freq))))
        (mul amp e)
        (pan 0.0)
        )))))


(defn ringm 
  "Simple instrument with ring-modulation"
  ([freq amp]
   (let-s [e (adsr 1.0 2.0 0.0 0.0)] 
     (->
       (ringmod 
         (blit-saw freq)
         (sine2 (mul freq 2.0)))
       (mul amp e)
       (pan 0.0)
       ))))


;; ================= 
;; AUDIO GRAPH SETUP 
;; =================

(def dry-node (create-node))
(def reverb-node (create-node))

(add-afunc (node-processor dry-node))
(add-afunc (node-processor reverb-node))


;; =====================
;; PERFORMANCE FUNCTIONS 
;; =====================

(defn perf-fm [afn]
  (let-s [sig (pan afn 0.05)] 
    (node-add-func
      dry-node (apply-stereo mul sig 0.7))
    (node-add-func
      reverb-node (apply-stereo mul sig 0.3))))

;; ===== 
;; SCORE
;; =====

(defn sieve-chord 
  ([base-pch sieve dur amp]
   (sieve-chord perf-fm base-pch sieve dur amp))
  ([instrfn base-pch sieve dur amp]
  (gen-notes 
    (repeat instrfn)
    0.0 dur (map #(pch->freq (pch-add base-pch %)) sieve) 
    amp)))

;; Set tempo of engine
(set-tempo 54)

;; Score in measured-score foringmat
(def score
  [:meter 4 4

  0.0 (sieve-chord [8 0] (gen-sieve 7 [2 0]) 3.0 0.25) 
  0.5 (sieve-chord [8 3] (gen-sieve 7 [2 0]) 3.0 0.25) 

  1.0 (sieve-chord [8 0] (gen-sieve 7 (U [4 0] [3 1])) 3.0 0.10) 
  1.1 (sieve-chord [8 3] (gen-sieve 7 (U [2 0] [3 1])) 3.0 0.10) 
  1.2 (sieve-chord [8 2] (gen-sieve 7 (U [3 0] [4 1])) 3.0 0.10) 
  1.3 (sieve-chord [8 1] (gen-sieve 7 (U [3 2] [4 0])) 3.0 0.10) 
  1.4 (sieve-chord [7 10] (gen-sieve 7 (U [3 0] [4 1])) 3.0 0.10) 
  1.5 (sieve-chord [7 8] (gen-sieve 7 (U [2 0] [3 1])) 3.0 0.10) 
  1.6 (sieve-chord [7 5] (gen-sieve 7 (U [4 0] [3 3] [11 5])) 3.0 0.10) 

  ;2.0 (sieve-chord ringm [8 3] (gen-sieve 7 [2 0]) 8.0 0.05) 

 ])

;(defn perf-event
;  [perf-func audio-event]
;  (event )
;  )

;; Utility functions for during development
(defn play-from [^double measure]
  (->>
    (convert-measured-score score)
    (starting-at (* measure 4))
    ;(sco->events)
    ;(add-audio-events)
    ))

(defn play-block [block]
  (add-audio-events (sco->events block)))

(defn render-to-disk [filename]
  (let [e (engine-create :nchnls 2)]
    (engine-set-tempo e 47)
    (->> (convert-measured-score score)
        (sco->events)
        (audio-events e)
        (engine-add-events e))
    (engine->disk e filename)
    ))



(comment

  (start-engine)

  ;; play from beginning
  (clear-engine)
  (play-from 0)
  (play-from 2)

  (stop-engine)

  )

