(ns music-examples.example2-advanced 
  (:require [pink.simple :refer :all] 
            [pink.io.midi :refer :all]
            [pink.config :refer :all]
            [pink.node :refer :all]
            [pink.space :refer :all]
            [pink.oscillators :refer :all]
            [pink.envelopes :refer [env xar adsr]]
            [pink.filters :refer [port butterlp moogladder]]
            [pink.delays :refer :all]
            [pink.util :refer :all]
            [primitive-math :refer [not==]])
  (:import [javax.sound.midi MidiSystem Transmitter Receiver MidiMessage
            ShortMessage ]
           [java.util Arrays]
           [clojure.lang IFn]))

;; Example 2 - Advanced - MIDI Keyboard (more developed)

; Create a Pink MIDI Manager
(def midim (create-midi-manager))

; Register a virtual device
(def keyboard (add-virtual-device midim "keyboard 1")) 

;; Setup reader functions for MIDI Control Change values
(defn midi-atom-reader
  "Read from atom assigned to MIDI CC values in Pink MIDI System.
  Scales values into target min and max range."
  [source-atom ^double target-mn ^double target-mx]
  (let [out ^doubles  (create-buffer)
        cur-val  (atom @source-atom)
        target-range  (- target-mx target-mn)]
    (fn []
      (let [v @source-atom] 
        (when (not  (= @cur-val v))
          (let [new-v (+ target-mn (* target-range (/  (double v) 127.0)))]
            (reset! cur-val v) 
            (Arrays/fill out new-v))))
      out)))

;; Grab value atoms for MIDI CC from Virtual Device
(def patch-number (get-midi-cc-atom keyboard 0 1))
(def cutoff-depth (get-midi-cc-atom keyboard 0 2))
(def resonance (get-midi-cc-atom keyboard 0 3))

;; Create shared, smoothing UGen readers for atoms 
(def cdepth-fn (shared (port (midi-atom-reader cutoff-depth 0.0 16.0) 0.05)))
(def resonance-fn (shared (port (midi-atom-reader resonance 0.0 1.0) 0.05)))


;; Synthesizer Note Instance Function
;; Frequency Modulation
(defn fm
  "Simple frequency-modulation sound with 1:1 cm ratio"
  [^double freq amp]
  (let [fm-index 0.9 
        mod-mult 1.0 
        mod-freq (* freq mod-mult)]
    (let-s [e (adsr 0.04 0.03 0.9 3.0)] 
      (->
        (sine2 (sum freq (mul freq fm-index e 
                              (sine2 mod-freq))))
        (mul amp e)
        (pan 0.0)
        ))))


;; Additive Synthesis
(defn additive-filter 
  [cutoff freq]
  (if (< freq cutoff)
    1.0
    (let [v (- freq cutoff)
          mx (* 16.0 cutoff)
          r (- mx cutoff)] 
      (max 0.0 (- 1.0 (/ v r))))))

(defn additive
  "Additive synthesis instrument, generates up to 32 partials"
  [freq amp]
  (let [num-partials (min (long (/ 10000 freq)) 32)
        cutoff 400] 
    (->
      (apply sum 
             (map #(let [f (* freq %)] 
                     (mul (additive-filter cutoff f) (sine f))) 
                  (range num-partials)))
      (mul amp (adsr 0.05 0.3 0.9 4.0) (/ 3.0 num-partials))
      (pan 0.0))))

;; Subtractive Synthesis
(defn subtractive 
  [freq amp]
  (let [ampfn (mul amp (adsr 0.05 0.3 0.9 4.0))] 
    (let-s [f (sum freq (mul freq 0.0025 (sine 4)))] 
      (-> 
        (sum (blit-saw (mul f 1.5)) 
             (blit-square f) 
             (blit-square (mul f 0.9995)))  
        (moogladder 
          (sum freq (mul cdepth-fn freq ampfn)) resonance-fn)
        (mul ampfn) 
        (pan 0.0)
        )))) 


;; Effects
(defn ping-pong-delay
  "Creates a stereo ping-pong delay given a mono audio function."
  [afn left-delay-time left-amp-mod
   right-delay-time right-amp-mod]
  (let [^"[[D" out (create-buffers 2) 
         ain (shared afn) 
         lfeedback (create-buffer 0.0)
         rfeedback (create-buffer 0.0)
         ldelay (->
                  (feedback-read rfeedback)
                  (mul left-amp-mod) 
                  (sum ain)
                  (adelay left-delay-time)
                  (feedback-write lfeedback))
         rdelay  (->
                  (feedback-read lfeedback)
                  (mul right-amp-mod) 
                  (sum ain)
                  (adelay right-delay-time)
                  (feedback-write rfeedback))] 
    (fn []
      (let [aleft (ldelay)
            aright (rdelay)]
        (when (and aleft aright)
          (aset out 0 aleft) 
          (aset out 1 aright) 
          out)))))

;; Bind hardware device to virtual MIDI device
  (bind-device midim "MPKmini2" "keyboard 1")

;; Create vector of patches to choose from
  (def patches 
    [fm additive subtractive])

;; setup ping-pong delay graph
  (def sub-node  (create-node))
  (def sub-node-processor (shared (node-processor sub-node)))

  (add-afunc (pan sub-node-processor 0.0))
  (add-afunc (ping-pong-delay sub-node-processor 
                               0.5 0.9 0.25 0.8))

;; Bind MIDI Keyboard Events to function
(bind-key-func
  keyboard 0
  (let [active ^"[[Z" (make-array Boolean/TYPE 128 1)] 
    (fn [cmd note-num velocity]
      (condp = cmd
        ShortMessage/NOTE_ON
        (let [done (boolean-array 1 false)
              patch-num (min 2 (/ (long @patch-number) 30)) 
              patch-fn (nth patches patch-num)
              afn (binding [*done* done] 
                    (patch-fn (midi->freq note-num) 
                           (/ (double velocity) 127.0)))]
          (aset active note-num done)
          (add-afunc afn)                         ;; <= add-afunc 
          ;(node-add-func sub-node afn) 
          
          )

        ShortMessage/NOTE_OFF
        (when-let [^booleans done (aget active note-num)]
          (aset done 0 true)
          (aset active note-num nil)))
      )))

;; Start engine for performance
(start-engine)

