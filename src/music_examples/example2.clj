(ns music-examples.example2
  (:require 
    [score.core :refer :all]
    [score.bpf :refer :all]
    [score.freq :refer :all])  
  (:require [pink.engine :refer :all]
            [pink.config :refer :all]
            [pink.envelopes :refer [env exp-env adsr xadsr xar]]
            [pink.oscillators :refer [oscili]]
            [pink.util :refer [mul sum let-s reader const create-buffer fill]]
            [pink.event :refer :all]
            [pink.config :refer :all]))


(defn table-synth [amp freq dur]
  (mul
     (oscili amp freq)
     (env [0.0 0.0, 0.05 1, 0.02 0.5, dur 0.5, 0.2 0])))


(defn score->events
  [score]
  (map #(apply event %) score))

(defn schedule 
  [e notes]
  (->> (score->events notes)
       (engine-events e)
       (engine-add-events e)))

(defn white-noise []
  (let [out (create-buffer)] 
    (fn []
      (fill out  (double-array 1 0)
            (fn [a] (- (* 2 (Math/random) 1)))))))

(defn wandering []
  (let [samp-wander 40000
        cur-count (atom 0)
        num-samples (atom (+ samp-wander (rand-int samp-wander)))
        cur-val (atom 0.5)
        cur-slope (atom (/ 0.5 @num-samples))
        out ^doubles (create-buffer)
        len *buffer-size*]
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
            (recur i new-run 0 v new-slope)))))))

(defn my-score [e amp base-freq] 
  (gen-notes
    (repeat table-synth)
    0.0 
    amp 
    (map #(mul (* % base-freq) 
               (sum 1.0 (wandering)))
         (range 1 3 0.5))
    20.0))

(defn my-score2 [e amp base-freq] 
  (let-s [w (wandering)]
    (gen-notes
      (repeat table-synth)
      0.0 
      amp 
      (map #(mul (* % base-freq) 
                 (sum 1.0 w))
           (range 1 3 0.5))
      20.0)))

(defn my-score3 [e amp base-freq] 
  (loop [indx 0 start 0.0 dur 40.0 score []] 
    (let-s [w (env [0.0 1.0 (- dur 6) 2.0 6 2.0])
            amp-env (env [0.0 0.0 (- dur 6) amp 5 amp 1.0 0.0])]  
      (if (< indx 5) 
        (recur (unchecked-inc indx) (+ start 5.0) (- dur 5.0) 
               (concat score (gen-notes
                 (repeat table-synth) 
                 start 
                 (repeat amp-env) 
                 (map #(mul (* % base-freq (inc indx)) w) (range 1 3 0.5))
                 dur)))
        score))))

(comment

  (def e (engine-create))  
  (engine-start e)

  ;; Simple Notes
  (schedule e (gen-notes (repeat table-synth)
                         (range 0 7 0.5)
                         0.2 
                         (range 440 1760 110) 
                         0.5
                         )) 

  ;; Time Vary Frequencies 
  (schedule e (my-score2 e 0.1 440.0))
  (schedule e (my-score e 0.1 440.0))

  (schedule e (my-score2 e (repeatedly #(mul 0.1 (wandering))) 220.0))
  (schedule e (my-score e (repeatedly #(mul 0.1 (wandering))) 110.0))
  
  (engine-clear e)  

  ;; Group Glissandi
  (schedule e (my-score3 e 0.1 110.0))
  
  (engine-stop e)
  (engine-kill-all)

  )





