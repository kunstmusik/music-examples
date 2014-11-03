(ns music-examples.generator
  (:require [pink.util :refer :all]
            [pink.config :refer :all]))

(comment

  ;; Generator Macro
  ;; * Four Arguments 
  ;;   * [loop vars]
  ;;   * [for x in ugen signals]
  ;;   * (loop body)
  ;;   * (yield value)
  ;; 
  ;; - creates an audio function 
  ;; - builds upon Clojure's loop-recur 
  ;; - creates, tracks, and saves state vars  
  ;; - nil checks and short circuits
  ;;   for you for afunc dependencies 
  ;; - handles retrieving of values from buffers 

  (defn phasor 
    "Phasor with fixed frequency and starting phase"
    [^double freq ^double phase]
    (let  [phase-incr ^double  (/ freq  (double *sr*))
           out ^doubles  (create-buffer)]
      (generator 
        [cur-phase phase]
        []
        (do
          (aset out indx cur-phase)
          (recur  (unchecked-inc indx)  (rem  (+ phase-incr cur-phase) 1.0)))
        (yield out))))

  (defn sine 
    "Sine generator with fixed frequency and starting phase"
    ([^double freq]
     (sine freq 0.0))
    ([^double freq ^double phase]
     (let  [phsr  (phasor freq phase)
            out ^doubles  (create-buffer)]
       (generator 
         []  
         [phs phsr]
         (let  [v  (Math/sin  (* TWO_PI phs))]
           (aset out indx v) 
           (recur (unchecked-inc indx)))          
         (yield out)))))


  ;; macro-expanded and reformatted

  (defn phasor  
    [freq phase]  
    (let [phase-incr (/ freq (double *sr*)) 
          out (create-buffer)]  
      (let [state1794 (double-array 1 [phase]) 
            buffer-size1795  (long *buffer-size*)]  
        (fn []  
          (let [] 
            (if true 
              (loop [indx 0 
                     cur-phase (aget state1794 0)]  
                (if (< indx buffer-size1795)  
                  (let []  
                    (do  
                      (aset out indx cur-phase)  
                      (recur (unchecked-inc indx)  
                             (rem (+ phase-incr cur-phase) 1.0))))  
                  (do 
                    (aset state1794 0 cur-phase) 
                    out)))))))))


  (defn sine  
    ([freq]  (sine freq 0.0))  
    ([freq phase]  
     (let [phsr (phasor freq phase) 
           out (create-buffer)]  
       (let [buffer-size1799 (long *buffer-size*)]  
         (fn []  
           (let [buffer1798 (phsr)]  
             (if buffer1798  
               (loop [indx 0]  
                 (if (< indx buffer-size1799)  
                   (let [phs (aget buffer1798 indx)]  
                     (let [v (Math/sin (* TWO_PI phs))]  
                       (aset out indx v)  
                       (recur (unchecked-inc indx))))  
                   (do out))))))))))

  )
