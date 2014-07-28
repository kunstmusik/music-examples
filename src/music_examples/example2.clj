(ns music-examples.example2
  (:require 
    [score.core :refer :all]
    [score.bpf :refer :all]
    [score.freq :refer :all])  
  (:require [pink.audio.engine :as eng]
            [pink.audio.envelopes :refer [env exp-env adsr xadsr xar]]
            [pink.audio.oscillators :refer [oscili]]
            [pink.audio.util :refer [mul sum let-s reader const create-buffer fill]]
            [pink.event :refer :all]))


(defn table-synth [amp freq dur]
  (mul
     (oscili amp freq)
     (env [0.0 0.0, 0.05 1, 0.02 0.5, dur 0.5, 0.2 0])))

(comment

  (defn print-assembly 
    [obj]
    (println (str "Printing assembly for class: " (.getClass obj)))
    ()
    )

  (print-assembly (table-synth 1 0 2 ))

  )

(defn score->events
  [score]
  (map #(apply event %) score))

(defn schedule 
  [e notes]
  (engine-events e 
    (score->events notes)))


(defn white-noise []
  (let [out (create-buffer)] 
    (fn []
      (fill out  (double-array 1 0)
            (fn [a] (- (* 2 (Math/random) 1)))) 
      ))
  )

(defn wandering []
  (let [samp-wander 12000
        cur-count (atom 0)
        num-samples (atom (+ samp-wander (rand-int samp-wander)))
        cur-val (atom 0.5)
        cur-slope (atom (/ 0.5 @num-samples))
        out ^doubles (create-buffer)
        len eng/*ksmps*]
    (fn []
      (loop [i (unchecked-int 0)
             samps @num-samples
             cnt (unchecked-int @cur-count)
             v ^double @cur-val
             incr ^double @cur-slope]
        (if (< cnt samps)
          (if (< i len) 
            (let [new-v ^double (+ v incr)] 
              (aset out i new-v)
              (recur (unchecked-inc-int i)
                     samps (unchecked-inc-int cnt) new-v incr))
            (do 
              (reset! cur-count cnt)
              (reset! num-samples samps)
              (reset! cur-val v)
              (reset! cur-slope incr)
              out)) 
          (let [new-run (+ samp-wander (rand-int samp-wander))
                new-target (rand)
                new-slope (/ (- new-target v) new-run)] 
            (recur i new-run 0 v new-slope))
          )))))

(defn my-score [e] 
  (schedule e
            (gen-notes
              (repeat table-synth)
              0.0 
              0.1
              (map #(mul (const (* % 440.0)) 
                         (sum 1.0 (wandering)))
                   (range 1 3 0.5))
              55.0)))

(defn my-score2 [e] 
  (let-s [w (wandering)]
   (schedule e
            (gen-notes
              (repeat table-synth)
              0.0 
              0.1
              (map #(mul (const (* % 440.0)) 
                         (sum 1.0 w))
                   (range 1 3 0.5))
              55.0))))


(comment

  (def e (eng/engine-create))  
  (eng/engine-start e)

  (eng/engine-add-afunc e (eng-events-runner (my-score e)))
  (eng/engine-add-afunc e (eng-events-runner (my-score2 e)))

  (binding [eng/*ksmps* 256]
      (eng/engine-add-afunc e (eng-events-runner (my-score e)))
    )

  (eng/engine-stop e)
  (eng/engine-clear e)  

  (let [e (eng/engine-create)
        simple-glissandi (my-score e) 
        ]
    
      (eng/engine-start e)
      (eng/engine-add-afunc e (eng-events-runner simple-glissandi))

      (Thread/sleep 6000)
      (eng/engine-stop e)
      (eng/engine-clear e)))



