(ns music-examples.example1aa 
  (:require [pink.simple :refer :all] 
            [pink.io.midi :refer :all]
            [pink.config :refer :all]
            [pink.space :refer :all]
            [pink.oscillators :refer :all]
            [pink.envelopes :refer [env xar adsr]]
            [pink.filters :refer [port butterlp moogladder]]
            [pink.util :refer :all]
            [primitive-math :refer [not==]]
            )
  (:import [javax.sound.midi MidiSystem Transmitter Receiver MidiMessage
            ShortMessage ]
           [java.util Arrays]
           [clojure.lang IFn]))

;; Example 1aa - Basic Engine Use - Add/Remove Audio Functions
;;   MIDI Keyboard (more developed)

(def midim (create-midi-manager))
(def keyboard (add-virtual-device midim "keyboard 1")) 

(defn midi-atom-reader
  [source-atom ^double target-mn ^double target-mx]
  (let  [out ^doubles  (create-buffer)
         cur-val  (atom @source-atom)
         target-range  (- target-mx target-mn)]
    (fn  []
      (let  [v @source-atom] 
        (when  (not  (= @cur-val v))
          (let  [new-v  (+ target-mn  (* target-range  (/  (double v) 127.0)))]
            (reset! cur-val v) 
            (Arrays/fill out new-v))))
      out)))


(def cutoff-freq (get-midi-cc-atom keyboard 0 1))
(def cutoff-depth (get-midi-cc-atom keyboard 0 2))
(def resonance (get-midi-cc-atom keyboard 0 3))

(def cfreq-fn (shared (port (midi-atom-reader cutoff-freq 0.0 1.0) 0.05)) )
(def cdepth-fn (shared (port (midi-atom-reader cutoff-depth 0.0 16.0) 0.05)) )
(def resonance-fn (shared (port (midi-atom-reader resonance 0.0 1.0) 0.05)))


;; Synthesizer Note Instance Function
(defn saw
  [freq amp]
  (let [ampfn (mul amp (adsr 0.05 0.3 0.9 4.0))] 
    (let-s [f (sum freq (mul freq 0.0025 (sine 4)))] 
      (pan (mul ampfn 
                (moogladder 
                  (sum (blit-saw (mul f 1.5)) 
                       (blit-square f)
                       (blit-square (mul f 0.9995)))
                  (sum freq (mul cdepth-fn freq ampfn))    
                  resonance-fn     
                  )) 
           0.0)))) 


  (bind-device midim "MPKmini2" "keyboard 1")

  (bind-key-func
    keyboard 0
    (let [active ^"[[Z" (make-array Boolean/TYPE 128 1)] 
      (fn [cmd note-num velocity]
        (condp = cmd
          ShortMessage/NOTE_ON
          (let [done (boolean-array 1 false)
                afn (binding [*done* done] 
                      (saw (midi->freq note-num) (/ velocity 127)))]
            (aset active note-num done)
            (add-afunc afn))                        ;; <= add-afunc

          ShortMessage/NOTE_OFF
          (when-let [^booleans done (aget active note-num)]
            (aset done 0 true)
            (aset active note-num nil)))
        )))

;; Start engine for performance
(start-engine)

