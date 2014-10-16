(ns music-examples.example2
  (:require 
    [score.core :refer :all]
    [score.bpf :refer :all]
    [score.freq :refer :all])  
  (:require [pink.simple :refer :all]
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
  [notes]
  (->> (score->events notes)
       (add-events)))

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

(defn my-score [amp base-freq] 
  (gen-notes
    (repeat table-synth)
    0.0 
    amp 
    (map #(mul (* % base-freq) 
               (sum 1.0 (wandering)))
         (range 1 3 0.5))
    20.0))

(defn my-score2 [amp base-freq] 
  (let-s [w (wandering)]
    (gen-notes
      (repeat table-synth)
      0.0 
      amp 
      (map #(mul (* % base-freq) 
                 (sum 1.0 w))
           (range 1 3 0.5))
      20.0)))

(defn my-score3 [amp base-freq] 
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

  (start-engine)

  ;; Simple Notes
  (schedule (gen-notes (repeat table-synth)
                         (range 0 7 0.5)
                         0.2 
                         (range 440 1760 110) 
                         0.5
                         )) 

  ;; Time Vary Frequencies 
  (schedule (my-score2 0.1 440.0))
  (schedule (my-score 0.1 440.0))

  (schedule (my-score2 (repeatedly #(mul 0.1 (wandering))) 220.0))
  (schedule (my-score (repeatedly #(mul 0.1 (wandering))) 110.0))
  
  (clear-engine)  

  ;; Group Glissandi
  (schedule (my-score3 0.1 110.0))
  
  (engine-stop e)
  (engine-kill-all))





