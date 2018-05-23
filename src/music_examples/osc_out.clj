(ns music-examples.osc-out
  (:require [pink.simple :refer :all]
            [pink.live-code :refer :all]
            [pink.config :refer :all]
            [pink.util :refer :all]
            [pink.node :refer :all]
            [pink.envelopes :refer :all]
            [pink.space :refer :all]
            [pink.filters :refer :all]
            [pink.oscillators :refer :all]
            [clojure.string :refer [join]]
            [score.core :refer :all]
            [score.freq :refer :all]
            [score.lc :refer :all]
            [score.euclid :refer :all]
            [score.beats :refer :all] 
            )
  (:import [de.sciss.net OSCClient OSCChannel OSCMessage]
           [java.net InetSocketAddress]
           ))


(def OSC-CLIENT
  (let [c (OSCClient/newUsing OSCChannel/UDP)] 
    (.setTarget c (InetSocketAddress. "127.0.0.1" 10005))
    (.start c)
    c ))

(defn osc-send [target & args]
  ;;(println target)
  (.send OSC-CLIENT (OSCMessage. target (object-array args))))

(defn synth1 
  [dur freq]
  (with-duration (beats dur) 
    (let [e (shared (adsr 0.01 0.05 0.25 0.25))]
      (->
        (sum (blit-saw freq)
             (blit-square (* freq 2)) ) 
        (zdf-ladder (sum 200 (mul 4000 (exp-env [0.0 1.0 dur 0.001] ))) 5)
        (mul e 0.75)
        (pan 0.0)))))

(defn hex 
  "Predicate that returns if pulse-num item in hex-str beat is 1"
  [hex-str pulse-num]
  (let [pat (hex->pat hex-str) 
        cnt (count pat)
        indx (rem pulse-num cnt)] 
    (== 1 (nth pat indx))))

(defn perf [pulse-num]
  ;; CALLBACK INSTRUMENT
  ;; same style of programming as in Steven's livecode.orc
  ;; Csound live coding framework

  (when (hex "a0" pulse-num)
    (osc-send "/i" 1 0.25 440 0.25))

  ;; set to true to hear Pink synth 
  (when false  
    (when (hex "a0" pulse-num)
      (add-afunc (synth1 0.25 220)) )

    (when (hex "2a" pulse-num)
      (add-afunc (synth1 0.25 330)) )

    (when (hex "8020" pulse-num)
      (add-afunc (synth1 0.5 55))))

  )

(def clock-pulse (long-array 1))

(defn clock []
  (let [last-pulse ^long (aget ^longs clock-pulse 0)
        cur-pulse (long (* (now) 4)) ]
    (when (not= last-pulse cur-pulse)
      (aset ^longs clock-pulse 0 cur-pulse) 
      (#'perf cur-pulse))
    true))




(comment
 
  ;; evaluate this in REPL to get started 
  (start-engine)
  (add-pre-cfunc clock)
  (set-tempo 132)

  )
