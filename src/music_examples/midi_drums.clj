(ns music-examples.midi-drums
  (:require [pink.simple :refer :all] 
            [pink.io.midi :refer :all]
            [pink.io.sound-file :refer :all]
            [pink.config :refer :all]
            [pink.space :refer :all]
            [pink.oscillators :refer :all]
            [pink.envelopes :refer [env xar]]
            [pink.filters :refer [port butterlp moogladder]]
            [pink.util :refer :all]
            [clojure.java.io :refer [resource]])
  (:import [javax.sound.midi MidiSystem Transmitter Receiver MidiMessage
            ShortMessage ]
           [java.util Arrays]
           [clojure.lang IFn]))

;;   MIDI Drum Machine (configured to work with pads on Akai MPK mini2)

(def midim (create-midi-manager))
(def keyboard (add-virtual-device midim "keyboard 1")) 

(def samples-root
  "/Users/stevenyi/work/csound/samples/salamanderDrumkit/OH/")

(defn table 
  [filename]
  (load-table (str samples-root filename)))

(def samples
  { 48 (table "kick_OH_F_1.wav")
    49 (table "snare_OH_F_1.wav")
    50 (table "ride2_OH_FF_1.wav")
    51 (table "hihatClosed_OH_F_1.wav")

    44 (table "hihatClosed_OH_F_1.wav")
    45 (table "hihatClosed_OH_F_1.wav")
    46 (table "hihatClosed_OH_F_1.wav")
    47 (table "cowbell_FF_1.wav")
   })

(defn get-duration 
  [wave]
  (let [d ^doubles (aget ^"[[D"(:data wave) 0)]
    (/ (alength d) (double *sr*))))

(defn play-sample-one-shot
  [^long keynum]
  (when-let [sample (samples keynum)] 
    (let [dur (get-duration sample)]
     (add-afunc                                     ;; add-afunc
      (pan 
        (mul (env [0 0 0.001 0.5 (- dur 0.002) 0.5 0.001 0])
             (oscili 1.0 (/ 1.0 dur) 
                     (aget ^"[[D" (:data sample) 0)))
        0.0
        )))))


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
          (play-sample-one-shot note-num)
          )
        )))


  (start-engine)                                    ;; start-engine
  
  )

