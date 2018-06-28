(ns music-examples.midi-out
  (:require [pink.simple :refer :all]
            [pink.live-code :refer :all]
            [pink.config :refer :all]
            [pink.util :refer :all]
            [pink.node :refer :all]
            [pink.envelopes :refer :all]
            [pink.space :refer :all]
            [pink.filters :refer :all]
            [pink.oscillators :refer :all]
            [pink.io.midi :as midi]
            [clojure.string :refer [join]]
            [score.core :refer :all]
            [score.freq :refer :all]
            [score.lc :refer :all]
            [score.euclid :refer :all]
            [score.beats :refer :all] 
            ))


(defn hex 
  "Predicate that returns if pulse-num item in hex-str beat is 1"
  [hex-str pulse-num]
  (let [pat (hex->pat hex-str) 
        cnt (count pat)
        indx (rem pulse-num cnt)] 
    (== 1 (nth pat indx))))

(defn xosc [phs values]
  (let [p (rem phs 1)
        indx (int (* p (count values)))]
    (nth values indx)))

(def midi-receiver (atom nil))

(defn perf [pulse-num]
  ;; CALLBACK INSTRUMENT
  ;; same style of programming as in Steven's livecode.orc
  ;; Csound live coding framework

  ;; set to true to hear MIDI output 
  (when-let [rcv @midi-receiver] 
    (when (hex "a" pulse-num)
      (add-pre-cfunc
        (midi/midi-note rcv (beats 0.25) 0 (+ 60 (rand-nth [0 4 7 11])) 120)))
    ))


(def clock-pulse (long-array 1))

(defn clock []
  (let [last-pulse ^long (aget ^longs clock-pulse 0)
        cur-pulse (long (* (now) 4)) ]
    (when (not= last-pulse cur-pulse)
      (aset ^longs clock-pulse 0 cur-pulse) 
      (try 
        (#'perf cur-pulse)
        
        ))
    true))




(comment
 
  ;; evaluate this in REPL to get started 
  (start-engine)
  (add-pre-cfunc clock)
  (set-tempo 132)

  ;; eval following to use first available MIDI output device on system
  (when-let [d (nth (midi/list-output-devices) 0)]
    (.open (:device d))
    (when-let [rcv (.getReceiver (:device d))]
     (reset! midi-receiver rcv)))
  
  ;; testing with Animoog on iPad (rtpMidi device uses DESKTOP on my Win10 system)
  (when-let [d (midi/find-device "DESKTOP" :out)]
    (.open (:device d))
    (when-let [rcv (.getReceiver (:device d))]
     (reset! midi-receiver rcv) 
      )
    )

  )
