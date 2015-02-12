(ns music-examples.track1 
  (:require [score.core :refer :all]
            [score.freq :refer :all])  
  (:require [pink.simple :refer :all]
            [pink.engine :refer :all]
            [pink.config :refer :all]
            [pink.control :refer :all]
            [pink.filters :refer :all]
            [pink.envelopes :refer :all]
            [pink.util :refer :all]
            [pink.oscillators :refer :all]
            [pink.space :refer :all]
            [pink.event :refer :all]))

;; Example Track 1
;; 
;; Defines a score to play using Score's measured-score format. The score is
;; converted into Pink events and played by the pink.simple engine.  
;;
;; Note: To run this, first evaluate the code in a REPL using require, then
;; evaluate the code within the comment block near the end of the file.
;; After calling start-engine, you can use the play-from lines to start 
;; playing back from a given measure number. 


;; Base Instrument Definitions
(defn instr-square
  [amp freq loc]
  (let-s [e (xar 0.01 0.9)] 
    (->
      (sum (blit-square freq))
      (butterlp (sum 100 (mul e 400)))
      (mul e amp)
      (pan loc))))

(defn instr-square2
  [amp freq loc]
  (let-s [e (adsr 0.01 0.6 0.0 0.00)] 
    (->
      (sum (blit-square freq))
      (butterlp (sum 100 (mul e 400)))
      (mul e amp)
      (pan loc))))

(defn fm
  "Simple frequency-modulation sound with default 1.77:1 cm ratio"
  ([freq amp]
   (fm freq amp 0.4 1.77))
  ([freq amp fm-index mod-mult]
  (let [mod-freq (mul freq mod-mult)]
    (let-s [e (xar 0.02 2.0)] 
      (->
        (sine2 (sum freq (mul freq fm-index e 
                              (sine2 mod-freq))))
        (mul amp e)
        (pan 0.0)
        )))))

(defn fm-bass
  "fm-bass 2:1"
  ([freq amp]
  (let [mod-freq (mul 2.0 freq) 
        fm-index 1.0]
    (let-s [e (adsr 0.0 0.450 0.0 0.05)] 
      (->
        (sine2 (sum freq (mul freq fm-index  
                              (sine2 mod-freq))))
        (mul amp e)
        (pan -0.16)
        )))))

(defn subtractive 
  [freq amp cutoff res]
  (let [ampfn (mul amp (adsr 0.05 0.3 0.9 4.0))] 
    (let-s [f (arg freq) ;(sum freq (mul freq 0.0025 (sine 4)))
            ] 
      (-> 
        (sum (blit-saw (mul f 1.5)) 
             (blit-square f))  
        (moogladder cutoff res)
        (mul ampfn) 
        (pan 0.0)
        ))))

(defn subtractive2 
  [freq amp cutoff res]
  (let [ampfn (mul amp (adsr 0.1 0.3 0.9 0.2))] 
    (let-s [f (sum freq (mul freq 0.0025 (sine 4)))] 
      (-> 
        (sum (blit-saw f) 
             (mul 0.5 (blit-square (mul f 0.5))) )  
        (moogladder cutoff res)
        (mul ampfn) 
        (pan 0.2)
        ))))

(defn subtractive3
  [freq amp cutoff]
  (let [ampfn (adsr 0.05 0.3 1.0 0.1)] 
    (let-s [f (sum freq (mul freq 0.0025 (sine 4)))] 
      (-> 
        (sum (blit-saw (mul f 1.5)) 
             (blit-saw f) 
             (blit-saw (mul f 0.9995)))  
        (butterlp cutoff)
        (mul ampfn amp) 
        (pan -0.015)
        ))))


(defn sub-growl 
  [freq amp cutoff res space]
  (let [ampfn (mul amp (adsr *duration* 0.0 0.0 0.1))] 
    (let-s [f (sum freq (mul freq 0.0025 (sine 4)))] 
      (-> 
        (sum (blit-saw (mul f 1.5)) 
             (blit-square f) )  
        (moogladder cutoff res)
        (mul ampfn) 
        (pan space)
        ))))

;; perf instruments

(defn bass-drum
  [amp]
  (instr-square amp (env [0.0 100 0.1 40 2.0 40]) 0.0))

(defn subtractor 
  [notename amp cutoff res]
  (subtractive (keyword->freq notename) amp cutoff res))

(defn subtractor2
  [notename amp cutoff res]
  (subtractive2 (keyword->freq notename) amp cutoff res))


(defn growl 
  [notename amp cutoff res space]
  (sub-growl (keyword->freq notename) amp cutoff res space))

(defn wobble
  [center mod-width freq]
  (-> (sine2 freq)
      (mul mod-width)
      (sum center)))

;; ===== 
;; SCORE
;; =====

(def drum-beat
  [[bass-drum 0.0 0.25 1.0]
   [fm 0.5 0.3 700 0.3]
   [bass-drum 2.0 0.25 1.0]
   [fm 2.5 0.3 1000 0.3]])

(def pat-base
  (map #(into [instr-square2] %)
   [[0.0 0.25 0.25 300 -0.2] 
   [0.0 0.25 0.25 400 0.1] 
   [0.5 0.25 0.15 300 -0.2] 
   [0.5 0.25 0.15 400 0.1] 
   [1.0 0.25 0.10 300 -0.2] 
   [1.0 0.25 0.10 400 0.1]]))

(def pat
  (concat 
    (with-start 0.5 pat-base)
    (with-start 2.0 pat-base)))

(def drum-beat-pat (concat drum-beat pat))

;; melodic parts

(def pentatonic
  [[subtractor 0.0 4.0 :G5 0.2 2000 0.15] 
   [subtractor 4.0 4.0 :F5 0.2 2000 0.15] 
   [subtractor 8.0 4.0 :D5 0.2 2000 0.15] 
   [subtractor 12.0 4.0 :C5 0.2 2000 0.15] 
   [subtractor 16.0 4.0 :G4 0.2 2000 0.15] 
   ])

(def growing-line
  (let [e (!*! env [0.0 400 0.11 5000])
        starts (range 0 1.8 (/ 1.0 3.0))
        amps (range 0.05 5 0.05)
        space (range 0.75 -1.0 -0.25)
        ] 
    (map #(into [growl] %) 
         (concat
           (gen-notes starts 0.1 :G5 amps e 0.75 space) 
           (gen-notes starts 0.1 :G3 amps e 0.75 space)
           
           ))))

;(play-block growing-line)


(def bass-line 
  (map #(into [fm-bass] %)
    (gen-notes [0.0 0.25 1.0 2.0 2.25 3.0 3.5]
               0.1 100 0.4)))

(def bass-line-4
  (convert-measured-score 
    [:meter 4 4
    0 bass-line                      
    1 bass-line                      
    2 bass-line                      
    3 bass-line]))


(def line0
  (map #(into [subtractor2] %)
    [[0.5 0.2 :F6 0.2 3000 0.5 ]
     [1.0 0.3 :F6 0.2 3000 0.5 ]
     [1.375 0.1 :D6 0.2 3000 0.5 ]
     [1.5 0.4 :E6 0.2 3000 0.5 ]
     [2.0 0.3 :F6 0.2 3000 0.5 ]
     [2.375 0.1 :D6 0.2 3000 0.5 ]
     [2.5 0.4 :E6 0.2 3000 0.5 ]

     [3.0 0.2 :F6 0.2 3000 0.5 ]
     [3.5 0.3 :F6 0.2 3000 0.5 ]
     [3.875 0.1 :D6 0.2 3000 0.5 ]
     [4.0 0.4 :E6 0.2 3000 0.5 ]
     ;[4.5 0.3 :F6 0.2 3000 0.5 ]
     ;[4.875 0.1 :D6 0.2 3000 0.5 ]
     ;[5 0.4 :E6 0.2 3000 0.5 ]
     ]
    ))


(def melodic1
  (as-> 
    [[0.0 0.2 :G6 0.1 0.4 2.0]
      [0.5 0.2 :Bb6 0.1 0.4 2.0]
      [1.0 0.2 :D7 0.1 0.4 2.0]
    ] sco  
    (map #(into [fm] %) sco)
    (process-notes sco 3 keyword->freq)))


(def wah1
  [[subtractive3 0.0 8.0 
    (!*! env [0.0 300 8.0 400]) 
    (!*! env [0.0 0.0 0.5 0.1 4.0 0.0]) 
    ;(!*! wobble 2000 750 0.5) 
    2000]
   [subtractive3 0.0 8.0 
    (!*! env [0.0 500 8.0 (* 1.333 500)]) 
    (!*! env [0.0 0.0 0.5 0.1 4.0 0.0]) 
    ;(!*! wobble 2000 750 0.5) 
    1500]
   ])
;(play-block wah1)

;; Set tempo of engine
(set-tempo 47)

;; Score in measured-score format
(def score
  [:meter 4 4

   ;0 [:repeat 16 drum-beat] 
   ;1 [:repeat 8 :every 2 pat]  
   ;;  

   0 drum-beat  
   1 drum-beat 
   2 drum-beat-pat   
   3 drum-beat-pat 
   ;; 
   4 drum-beat-pat pentatonic 
   5 drum-beat-pat
   6 drum-beat-pat    
   7 drum-beat-pat
   ;;

   8  drum-beat-pat bass-line-4
   9  drum-beat-pat 
   10 drum-beat-pat 
   11 drum-beat-pat 

   ;;
   12 drum-beat-pat pentatonic bass-line-4

   13 drum-beat-pat 
      [[subtractor 0.0 5.0 :Eb2 (!*! env [0.0 0.3 7.0 0.0]) 
        (!*! wobble 1000 750 0.5) 0.25]]
   14 drum-beat-pat    
   15 drum-beat-pat

   ;;
   16 drum-beat-pat growing-line bass-line-4
   17 drum-beat-pat 
   18 drum-beat-pat growing-line
   19 drum-beat-pat

   ;;
   20 drum-beat-pat pentatonic bass-line-4
   21 drum-beat-pat
      [[subtractor 0.0 5.0 :E2 (!*! env [0.0 0.3 7.0 0.0]) 
        (!*! wobble 1000 750 3) 0.25]
       [subtractor 2.0 5.0 :G2 (!*! env [0.0 0.3 7.0 0.0]) 
        (!*! wobble 1000 750 0.75) 0.25]]
   22 drum-beat-pat    
   23 drum-beat-pat
      [[subtractor 1.5 4.0 :E6 0.2 
        (!*! wobble 1000 750 
             (!*! mul 2 (!*! env [0 0.0 2.0 1.5 5.0 0.0]))) 0.25]]

   ;;
   24 drum-beat-pat line0 bass-line-4 
   25 drum-beat-pat (with-start 2 line0) 
   26 drum-beat-pat   
   27 drum-beat-pat line0

   ;;
   28 drum-beat-pat pentatonic (with-start 2 line0) bass-line-4
   29 drum-beat-pat
      [[subtractor 0.0 5.0 :E2 (!*! env [0.0 0.3 7.0 0.0]) 
        (!*! wobble 1000 750 3) 0.25]
       [subtractor 2.0 5.0 :Db2 (!*! env [0.0 0.3 7.0 0.0]) 
        (!*! wobble 1000 750 0.75) 0.25]]
   30 drum-beat-pat (with-start 0.5 melodic1)    
   31 drum-beat-pat
      [[subtractor 1.5 4.0 :E6 0.1 
        (!*! wobble 1000 750 
             (!*! mul 2 (!*! env [0 0.0 2.0 1.5 5.0 0.0]))) 0.25]]

   ;;
   32 drum-beat-pat line0 bass-line-4
   33 drum-beat-pat (with-start 2 line0) 
   34 drum-beat-pat   
   35 drum-beat-pat line0

   36 drum-beat-pat pentatonic bass-line-4 
   37 drum-beat-pat 
      [[subtractor 0.0 5.0 :E2 (!*! env [0.0 0.3 7.0 0.0]) 
        (!*! wobble 1000 750 3) 0.25]
       [subtractor 2.0 5.0 :Db2 (!*! env [0.0 0.3 7.0 0.0]) 
        (!*! wobble 1000 750 0.75) 0.25]]
   38 drum-beat-pat (with-start 0.5 melodic1)   
   39 drum-beat-pat
      [[subtractor 1.5 4.0 :E6 0.2 
        (!*! wobble 1000 750 
             (!*! mul 2 (!*! env [0 0.0 2.0 1.5 5.0 0.0]))) 0.25]]

   ;;
   40 drum-beat-pat bass-line-4 
      [[subtractor 0.0 5.0 :Eb2 (!*! env [0.0 0.3 7.0 0.0]) 
        (!*! wobble 1000 750 0.5) 0.25]]
   41 drum-beat-pat 
   42 drum-beat-pat    
      [[subtractor 0.0 5.0 :F2 (!*! env [0.0 0.3 7.0 0.0]) 
        (!*! wobble 1000 750 0.5) 0.25]]
   43 drum-beat-pat

   ;;
   44 drum-beat-pat bass-line-4
      [[subtractor 0.0 10.0 :G2 (!*! env [0.0 0.3 10.0 0.0]) 
        (!*! wobble 1000 750 0.5) 0.25]]
   45 drum-beat-pat 
   46 drum-beat-pat    
   47 drum-beat-pat

 ])


;; Utility functions for during development
(defn play-from [^double measure]
 (add-audio-events 
   (sco->events 
     (starting-at (* measure 4) 
      (convert-measured-score score)))))

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
(play-from 8)
(play-from 12)
(play-from 20)

(play-block pentatonic)
(play-block pentatonic)

  (stop-engine)

  )

