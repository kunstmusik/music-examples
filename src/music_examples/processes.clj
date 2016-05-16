(ns music-examples.processes 
 (:require [pink.simple :refer :all]
             [pink.event :refer :all] 
             [pink.instruments.piano :refer :all]
             [pink.instruments.horn :refer :all]
             [pink.util :refer :all]
             [pink.filters :refer :all]
             [pink.node :refer :all]
             [pink.space :refer :all]
             [pink.config :refer :all]
             [pink.envelopes :refer [env adsr exp-env]]
             [pink.oscillators :refer [blit-saw blit-square blit-triangle]]
             [pink.processes :refer [process wait cue countdown-latch] :as p]
             [pink.control :refer [chain]]
             ))

;; Study in Lutoslawski-style aleatory (i.e., ad libitum). Cues used by
;; conductor process to signal performer processes.  Latch used by conductor to
;; wait for each of the initial processes to complete at least one iteration
;; before waiting to give initial cue.


(defn subtractive 
  [freq amp cutoff res]
  (let [ampfn (if (number? amp) 
                (mul amp (adsr 0.05 4.0 0.01 4.0))
                amp)] 
    (let-s [f (arg freq)] 
      (-> 
        (sum (blit-saw f) 
             (mul 0.5 (blit-saw (mul 0.999998 f)))
             (mul 0.25 (blit-saw (mul 1.0001 f))))  
        (moogladder cutoff res)
        (mul ampfn 0.3) 
        (pan 0.0)
        ))))



(defn instr 
  [amp key-num pan-val]
  (->
    (piano :duration *duration* :keynum key-num :amp amp)
    ;(mul (hold-until 0.5 1.0 (env [0.0 1.0 0.1 0.0])))
    (pan pan-val)
    ))

(defn perf-piano
  [start dur amp midi-key pan-val]
  (add-audio-events 
    (i instr start dur amp midi-key pan-val)))

(defn perf-until-cued
  [pitches durs pan-val cue]
  (process
    (loop []
      (when (not (p/has-cued? cue))
        (let [time-adj (+ 1 (* 0.2 (Math/random)))] 
          (loop [[p & p-r] pitches [d & d-r] durs]
            (when (and p d)
              (let [dur (* d time-adj)]                  
                (when (not= p :rest) 
                  (perf-piano 0 dur 0.15 (+ 72 p) pan-val))
                (wait dur)
                (recur p-r d-r)))))
        (recur)
        ))))

(defn perf-until-cued-signal-latch
  [pitches durs pan-val cue latch]
  (process
        (loop [cued-latch false]
          (when (not (p/has-cued? cue)) 
            (let [time-adj (+ 1 (* 0.2 (Math/random)))] 
                (loop [[p & p-r] pitches [d & d-r] durs]
                  (when (and p d)
                    (let [dur (* d time-adj)]                  
                      (when (not= p :rest) 
                        (perf-piano 0 dur 0.15 (+ 72 p) pan-val))
                      (wait dur))
                    (recur p-r d-r))))
              (when (not cued-latch)
                (p/count-down latch))
              (recur true)
            ))))

(defn transpose
  [v tr]
  (map #(if (number? %) (+ % tr) %) v))

(defn beats
  [^double v]
  (* v (/ 60.0 (double *tempo*))))

;; SETUP PERFORMER PROCESSES

  (def cue0 (cue))
  (def cue1 (cue))
  (def latch0 (countdown-latch 3))

  (def p1-proc0 
    (perf-until-cued-signal-latch 
      [0 2 3 0 2 3 :rest 6]
      [0.1 0.1 0.1 0.1 0.1 0.1 0.8 2.0]
      -0.3
      cue0 latch0))

  (def p1-proc1
    (perf-until-cued 
      [0 2 3 0 2 3 5 3 2 :rest -1]
      [0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.8 2.0]
      -0.3
      cue1))


  (def p2-proc0
    (perf-until-cued-signal-latch 
      (transpose [8 9 11 :rest 8 11 9 :rest] -24)
      [0.1 0.1 0.2 0.2 0.1 0.1 0.2 1.5 ]
      0.3
      cue0 latch0))


  (def p2-proc1
    (perf-until-cued 
      (transpose [8 9 11 8 9 11 :rest 8 11 9 :rest] -24)
      [0.1 0.1 0.1 0.1 0.1 0.2 0.2 0.1 0.1 0.2 1.5 ]
      0.3
      cue1))


  (def p3-proc0
    (perf-until-cued-signal-latch 
      (transpose [5 5 5 :rest 5 :rest 5 5  :rest] -12)
      [0.1 0.1 0.1 0.1 0.2 0.1 0.2 0.1 0.1 0.2]
      0.0
      cue0 latch0))


  (def p3-proc1
    (perf-until-cued 
      (transpose [5 5 5 :rest 5 :rest 5 5 :rest] -12)
      [0.1 0.1 0.1 0.2 0.1 0.2 0.1 0.1 0.2]
      0.0
      cue1))


  (def p1 (chain p1-proc0 p1-proc1))
  (def p2 (chain p2-proc0 p2-proc1))
  (def p3 (chain p3-proc0 p3-proc1))

  (def conductor 
    (process
      (add-pre-cfunc p3)
      (wait (+ 1 (Math/random)))
      (add-pre-cfunc p1)
      (wait (+ 1 (Math/random)))
      (add-pre-cfunc p2)
      (wait latch0)
      (wait 12.0)
      (p/signal-cue cue0)
      (wait 15.0)
      (p/signal-cue cue1)
      ))

(comment
  
  (start-engine)

  (add-pre-cfunc conductor)

  (add-audio-events
    (i subtractive 0.0 2.0 440 0.15 4000 0.1) 
    (i subtractive 0.5 2.0 880 0.15 4000 0.1) 
    (i subtractive 1.0 2.0 1760 0.15 4000 0.1) )


)
