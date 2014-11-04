(ns pink.demo.midi-keys
  (:require [pink.simple :refer :all] 
            [pink.io.midi :refer :all]
            [pink.config :refer :all]
            [pink.space :refer :all]
            [pink.oscillators :refer :all]
            [pink.envelopes :refer [env xar]]
            [pink.filters :refer [port butterlp moogladder]]
            [pink.util :refer :all]
            [primitive-math :refer [not==]]
            )
  (:import [javax.sound.midi MidiSystem Transmitter Receiver MidiMessage
            ShortMessage ]
           [java.util Arrays]
           [clojure.lang IFn]))

;; Example 1a - Basic Engine Use - Add/Remove Audio Functions
;;   MIDI Keyboard

(def midim (create-midi-manager))
(def keyboard (add-virtual-device midim "keyboard 1")) 

(defn saw
  [freq amp]
  (pan (mul amp 
            (blit-saw freq))
       0.0))

(def cutoff-freq (get-midi-cc-atom keyboard 0 1))
(def cutoff-depth (get-midi-cc-atom keyboard 0 2))
(def resonance (get-midi-cc-atom keyboard 0 3))

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

(def cfreq-fn (shared (port (midi-atom-reader cutoff-freq 0.0 1.0) 0.05)) )
(def cdepth-fn (shared (port (midi-atom-reader cutoff-depth 0.0 16.0) 0.05)) )
(def resonance-fn (shared (port (midi-atom-reader resonance 0.0 1.0) 0.05)))

(defn saw
  [freq amp]
  (let-s [f (sum freq (mul freq 0.0025 (sine 4)))] 
    (pan (mul amp 
              (moogladder 
                (sum (mul 0.25 (blit-saw (mul f 2.000)))
                     (blit-saw f)
                     (blit-saw (mul f 0.9995))
                     (sine2 (mul f 0.5)))
                (sum freq (mul cdepth-fn freq))    
                resonance-fn     
                )) 
         0.0))) 

(comment
  ;(bind-device midim "nanoKEY KEYBOARD" "keyboard 1")
  (bind-device midim "MPKmini2" "keyboard 1")

  (bind-key-func
    keyboard 0
    (let [active (make-array IFn 128)] 
      (fn [cmd note-num velocity]
        (println ">> " cmd " " note-num " " velocity)
        (condp = cmd
          ShortMessage/NOTE_ON
          (let [afn (saw (midi->freq note-num) (/ velocity 127))]
            (aset active note-num afn)
            (add-afunc afn))                        ;; <= add-afunc

          ShortMessage/NOTE_OFF
          (when-let [afn (aget active note-num)]
            (remove-afunc afn)                      ;; <= remove-afunc 
            (aset active note-num afn)))
        )))



  (start-engine)                                    ;; start-engine
  
  )

