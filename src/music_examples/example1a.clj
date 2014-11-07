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

; Create a Pink MIDI Manager
(def midim (create-midi-manager))

; Register a virtual device
(def keyboard (add-virtual-device midim "keyboard 1")) 

; Instrument function
(defn saw
  [freq amp]
  (pan (mul amp 
            (blit-saw freq))
       0.0))

(comment
  ;; Bind Hardware device to virtual device
  ;(bind-device midim "nanoKEY KEYBOARD" "keyboard 1")
  (bind-device midim "MPKmini2" "keyboard 1")

  ;; Bind key event function to virtual device
  (bind-key-func
    keyboard 0
    (let [active (make-array IFn 128)] 
      (fn [cmd note-num velocity]
        (println ">> " cmd " " note-num " " velocity)
        (condp = cmd
          ShortMessage/NOTE_ON
          (let [afn (saw (midi->freq note-num) (/ velocity 127.0))]
            (aset active note-num afn)
            (add-afunc afn))                        ;; <= add-afunc

          ShortMessage/NOTE_OFF
          (when-let [afn (aget active note-num)]
            (remove-afunc afn)                      ;; <= remove-afunc 
            (aset active note-num afn)))
        )))



  (start-engine)                                    ;; start-engine
  
  )

