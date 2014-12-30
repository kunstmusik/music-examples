(ns music-examples.grains
  (:require [score.core :refer :all]
            [score.freq :refer :all])  
  (:require [pink.simple :refer :all]
            [pink.config :refer :all]
            [pink.envelopes :refer [xar]]
            [pink.util :refer [mul]]
            [pink.instruments.horn :refer :all]
            [pink.space :refer :all]
            [pink.event :refer :all]))

(defn apply-tempo 
  [tempo beat-time]
  (* beat-time (/ 60.0 tempo)))

(defn instr-horn
  [freq loc]
  (pan 
    (mul (xar 0.01 0.04) 
         (horn 0.1 freq))
    loc))

(comment

  (start-engine)

  (def score 
    (take 1000 (map keyword->freq (cycle [:c4 :d4]))))

  (def score2 
    (take 1000 (map keyword->freq (cycle [:c4 :d4 :e4 :f#4]))))

  (def score3 
    (take 1000 (map keyword->freq (cycle [:c5 :d5 :e5 :f#5 :g#4]))))

  (def score4 
    (take 1000 (map keyword->freq (cycle [:c5 :d6 :e5 :f#6 :g#4]))))

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

  (defn perf-func 
    "control-function that plays score blocks over time"
    [loc dur [x & xs] tempo-atom]
    (when x
      (add-afunc (instr-horn x loc)) 
      (when xs
        (add-events [(event perf-func (apply-tempo @tempo-atom dur) loc dur xs (atom tempo-atom))]))
      ))


  (def tempo (atom 60.0))
  (def tempo2 (atom 60.0))


  (add-events (event perf-func 0.0 [0.25 0.5 score (atom tempo)]))
  (add-events (event perf-func 0.0 [0.0 0.25 score2 (atom tempo)]))
  (add-events (event perf-func 0.0 [-0.25 0.5 score3 (atom tempo)]))
  (add-events (event perf-func 0.0 [1 0.25 score4 (atom tempo2)]))
  (add-events (event perf-func 0.05 [-1 0.2 score4 (atom tempo2)]))

  (reset! tempo 80.0)


  (add-post-cfunc (tempo-change tempo 10 300.0))
  (add-post-cfunc (tempo-change tempo 4 200.0))
  (add-post-cfunc (tempo-change tempo 1 100.0))
  (add-post-cfunc (tempo-change tempo 4 30.0))


  (add-post-cfunc (tempo-change tempo2 10 300.0))
  (add-post-cfunc (tempo-change tempo2 4 200.0))
  (add-post-cfunc (tempo-change tempo2 1 100.0))
  (add-post-cfunc (tempo-change tempo2 4 30.0))

  (stop-engine)

  )
