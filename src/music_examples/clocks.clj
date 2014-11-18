(ns music-examples.clocks
  (:require [score.core :refer :all]
            [score.bpf :refer :all]
            [score.freq :refer :all])  
  (:require [pink.simple :refer :all]
            [pink.config :refer :all]
            [pink.filters :refer :all]
            [pink.envelopes :refer :all]
            [pink.util :refer :all]
            [pink.instruments.horn :refer :all]
            [pink.oscillators :refer :all]
            [pink.space :refer :all]
            [pink.event :refer :all]))

;; Instrument Definitions
(defn instr-square
  [amp freq loc]
  (let-s [e (xar 0.01 0.9)] 
    (->
      (blit-square freq)
      (butterlp (sum 100 (mul e 400)))
      (mul e amp)
      (pan loc))))

(defn fm
  "Simple frequency-modulation sound with 2:1 cm ratio"
  [^double freq amp]
  (let [fm-index 0.4 
        mod-mult 1.77 
        mod-freq (* freq mod-mult)]
    (let-s [e (xar 0.02 2.0)] 
    (->
      (sine2 (sum freq (mul freq fm-index e 
                            (sine2 mod-freq))))
      (mul amp e)
      (pan 0.0)
      ))))

(defn subtractive 
  [freq amp cutoff res]
  (let [ampfn (mul amp (adsr 0.05 0.3 0.9 4.0))] 
    (let-s [f (sum freq (mul freq 0.0025 (sine 4)))] 
      (-> 
        (sum (blit-saw (mul f 1.5)) 
             (blit-square f) 
             (blit-square (mul f 0.9995)))  
        (moogladder cutoff res)
        (mul ampfn) 
        (pan 0.0)
        ))))


;; Control Functions
(defn create-clock
  "Sample-accurate clock that triggers a trigger-fn according to the tempo 
  held within the tempo-atm.  When the time has been met, it will call the 
  given trigger-fn and truncate the running sample-count."
  [tempo-atm trigger-fn]
  (let [sr (double *sr*)
        buffer-size (long *buffer-size*)
        init-val (long (* sr (/ 60.0 (double @tempo-atm)))) 
        ^longs sample-count (long-array 1 init-val)]
    (fn []
      (let [num-samples-to-wait (long (* sr (/ 60.0 (double @tempo-atm))))
            cur-samp (aget sample-count 0)]
        (if (>= cur-samp num-samples-to-wait)
          (do 
            (aset sample-count 0 (rem cur-samp num-samples-to-wait))
            (trigger-fn))
          (aset sample-count 0 (+ cur-samp buffer-size))))  
      true
      )))

(defn pattern-player
  "When trigger, checks within vector in pattern-atom at the current counter
  and triggers a function if found. Current counter increments and wraps around
  each time this function is triggered."
  [pattern-atom]
  (let [counter (long-array 1 0)]
    (fn []
      (let [indx (aget counter 0)
            perf-fn (nth @pattern-atom indx)
            len (count @pattern-atom)]
        (when perf-fn 
          (perf-fn)) 
        (aset counter 0 (rem (inc indx) len))) 
     true)))

(start-engine)

(comment

  ;; Tempo: 47 BPM, times 4 to get triggers every 16th note
  (def tempo (atom (* 47.0 4)))

  ;; Pattern filled with vectors of fn's
  (def bd-pattern (atom []))

  ;; Performance Functions (to use as pattern triggers)
  (defn play-fn 
    [dur amp loc]
    (add-audio-events 
      (i instr-square 0.0 dur amp (env [0.0 100 0.1 40 (- dur 0.1) 40]) loc)))

  (defn play-fm
    [dur freq amp]
    (add-audio-events
      (i fm 0.0 dur freq amp)))


  (defn wahwah
    [dur notename amp cutoff res]
    (add-audio-events
      (i subtractive 0.0 dur (keyword->freq notename) amp cutoff res)))

  ;; Customizing trigger functions using partial
  (def bd (partial play-fn 1.0 0.6 0.0))

  (def fmfm (partial play-fm 0.3))

  ;; Set initial pattern
  (reset! 
    bd-pattern 
    [bd nil nil nil 
     nil nil nil nil
     bd nil nil nil
     nil nil nil nil])

  ;; Set modified pattern
  (reset! 
    bd-pattern 
    [bd nil (partial fmfm 700 0.2) nil 
     nil nil nil nil
     bd nil (partial fmfm 1000 0.2) nil
     nil nil nil nil])

  ;; Play subtractive synth notes using static cutoff 
  (wahwah 2.0 :C5 0.2 2000 0.25)
  (wahwah 2.0 :D5 0.2 2000 0.25)
  (wahwah 2.0 :F5 0.2 2000 0.25)


  ;; Play subtractive synth notes using dynamic cutoff 
  (wahwah 3.0 :Eb2 0.3 (sum 1000 (mul 750 (sine 0.5))) 0.25)
  (wahwah 3.0 :C2 0.3 (sum 1000 (mul 750 (sine 6))) 0.25)
  (wahwah 3.0 :Db5 0.3 (sum 1000 (mul 750 (sine 4))) 0.25)
  (wahwah 4.0 :E5 0.3 (sum 1000 (mul 750 (sine 2))) 0.25)


  ;; player is the trigger function for the clock
  (def player (pattern-player bd-pattern))

  ;; clock will trigger the player function at the given tempo
  (def clock (create-clock tempo player))

  ;; register the control function with the engine
  (add-post-cfunc clock)

  (remove-post-cfunc clock)

  (stop-engine)

  )

