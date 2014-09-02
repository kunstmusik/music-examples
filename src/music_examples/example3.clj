(ns music-examples.example3
  (:require [score.core :refer :all]
            [score.bpf :refer :all]
            [score.freq :refer :all])  
  (:require [pink.engine :refer :all]
            [pink.config :refer :all]
            [pink.envelopes :refer [env exp-env adsr xadsr xar]]
            [pink.oscillators :refer [oscili]]
            [pink.util :refer [mul sum let-s reader const create-buffer fill]]
            [pink.instruments.horn :refer :all]
            [pink.space :refer :all]
            [pink.event :refer :all]))

(defn apply-tempo 
  [beat-time]
  (* beat-time (/ 60.0 @tempo)))

(defn instr-horn
  [freq loc]
  (pan 
    (mul (xar 0.01 0.04) 
         (horn 0.2 freq))
    loc))

(defn perf-func 
  "control-function that plays score blocks over time"
  [loc dur [x & xs]]
  (when x
    (play (instr-horn x loc)) 
    (when xs
      (schedule (event perf-func (apply-tempo dur) loc dur xs)))
    ))

(def score 
  (take 1000 (map ->freq (cycle [:c4 :d4]))))

(def score2 
  (take 1000 (map ->freq (cycle [:c4 :d4 :e4 :f#4]))))

(def score3 
  (take 1000 (map ->freq (cycle [:c5 :d5 :e5 :f#5 :g#4]))))

(def score4 
  (take 1000 (map ->freq (cycle [:c5 :d6 :e5 :f#6 :g#4]))))


(defn tempo-change 
  [tatom seconds end-tempo]
  (let [cur-buf (atom 0) 
        end (/ (* seconds *sr*) *buffer-size*) 
        incr (/ (- end-tempo @tatom) end)]
    (fn []
      (when (< @cur-buf end)
        (swap! cur-buf inc)
        (swap! tatom + incr)
        true))))

(comment

  (def e (engine-create :nchnls 2))  
  (engine-start e)

  (def play (partial engine-add-afunc e))
  (def schedule (partial engine-add-events e))
  (def ->freq (comp midi->freq keyword->notenum))

  (def tempo (atom 60.0))


  (schedule (event perf-func 0.0 [0.25 0.5 score]))
  (schedule (event perf-func 0.0 [0.0 0.25 score2]))
  (schedule (event perf-func 0.0 [-0.25 0.5 score3]))
  (schedule (event perf-func 0.0 [1 0.25 score4]))
  (schedule (event perf-func 0.05 [-1 0.2 score4]))

  (reset! tempo 80.0)


  (engine-add-post-cfunc e (tempo-change tempo 10 300.0))
  (engine-add-post-cfunc e (tempo-change tempo 4 200.0))
  (engine-add-post-cfunc e (tempo-change tempo 1 100.0))
  (engine-add-post-cfunc e (tempo-change tempo 4 40.0))

  (engine-stop e)
  (engine-kill-all)  

  )
