(ns music-examples.live-code-study
  (:require [pink.simple :refer :all]
            [pink.live-code :refer :all]
            [pink.config :refer :all]
            [pink.filters :refer :all]
            [pink.envelopes :refer :all]
            [pink.util :refer :all]
            [pink.node :refer :all]
            [pink.oscillators :refer :all]
            [pink.space :refer :all]
            [pink.event :refer :all]
            [pink.noise :refer :all]
            [pink.effects.ringmod :refer :all]
            [pink.effects.reverb :refer :all]
            [pink.effects.distortion :refer :all]
            [pink.io.sound-file :refer :all]
            [clojure.string :refer [join]]
            [score.core :refer :all]
            [score.freq :refer :all]
            [score.lc :refer :all]
            [score.euclid :refer :all]
            ))

;; instr

;; Download Salamander from https://archive.org/details/SalamanderDrumkit and
;; place within PINK_RESOURCE_ROOT/samples
(def samples-root
  (str (System/getenv "PINK_RESOURCE_ROOT") 
  "/samples/salamanderDrumkit/OH/"))

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


(def bd 48)
(def snare 49)
(def ride 50)

(defn play-sample-one-shot
  ([^long keynum]
   (play-sample-one-shot keynum 1.0))
  ([^long keynum ^double amp-adj]
   (when-let [sample (samples keynum)] 
     (let [dur (sample-duration sample)]
       (add-afunc                               
         (-> 
           (sample-one-shot sample)
           (mul amp-adj)
           (pan 0.0))
         )))))

;; control


(defn of-range [^double n ^double min-val ^double max-val]
  (+ min-val (* n (- max-val min-val))))

(defn play-samp [samp-num pattern indx amp]
  (when (pattern indx)
    (play-sample-one-shot samp-num amp)))

(defn sub-beat [n]
  (* (now) n))

(def bd-pat #{0 4 8 12})
(def snare-pat #{2 4 12 15}) 

(defn drums []
  (let [n (beat-mod (sub-beat 4) 16)]
    (play-samp bd bd-pat n 2.0)
    (play-samp snare snare-pat n 1.0)
    ;(play-samp ride (into #{} (range 0 16 2)) n 0.35)
    )
  (cause drums (next-beat 1/4)))


(def reverb (create-node :channels 2) )
(def reverb-fn
  (freeverb (node-processor reverb) 0.8 0.25))

(add-afunc reverb-fn)


(defn synth1 
  [dur freq]
  (with-duration (beats dur) 
    (let [e (shared (adsr 0.01 0.05 0.25 0.25))]
      (->
        (sum (blit-saw freq)
             (blit-square (* freq 2)) ) 
        (zdf-ladder (sum 500 (mul 2000 e)) 0.75)
        (mul e 0.5)
        (pan 0.0)))))

(defn synth2 
  [dur freq]
  (with-duration (beats dur) 
    (let [e (shared (adsr 0.01 (beats 0.5) 0.001 (beats 0.5)))]
      (->
        (sum (blit-square freq)
             (blit-saw (* freq 2.00017)) ) 
        (k35-hpf 300 8)
        (k35-lpf (sum 300 (mul 
                               (of-range (let [v (/ (beat-mod 32) 16.0)]
                                           (if (> v 1.0) (- 2.0 v) v)) 
                                         200 5000) 
                               ;3000
                                  e)) 
                    3)
        (mul e 0.5)
        (pan 0.0)))))

(defn bass 
  [dur freq]
  (with-duration (beats dur) 
    (let [e (shared (adsr 0.01 (beats 0.5) 1.0 (beats 0.5)))]
      (->
        (blit-saw freq)
        (diode-ladder (sum 200 (mul 
                               (of-range (let [v (/ (beat-mod 32) 16.0)]
                                           (if (> v 1.0) (- 2.0 v) v)) 
                                         2000 16000) 
                               
                              (xar 0.01 (- dur 0.01))))
                      10 :norm 4)
        (mul e 6.0)
        (pan 0.0)))))


(comment
  (add-wet-dry 
    0.2  
    (-> (sum (blit-saw 400)
             (mul 0.5 (blit-saw 800)
                  (blit-saw 800.2317)))
        (zdf-ladder (sum 100 (mul 10000 (adsr 0.0 4.0 0 4.0))) 0.8)
        (mul 0.8)
        (pan 0.0)
        ))
  )


(defn add-wet-dry
  [wet-dry afn]
  (let [s (shared afn)]
    (add-afunc 
      (apply-stereo mul s (- 1.0 wet-dry)))
    (node-add-func 
      reverb 
      (apply-stereo mul s wet-dry)))) 

(defn m1
  [indx]
  (let [dur (rand-nth [1/4 1/2 1 1])
        freq (* 100 (inc indx)) 
        wet-dry 0.5]
    (add-wet-dry wet-dry (synth1 dur freq))
    (cause m1 (next-beat dur) 
           (mod (inc indx) 8)
           )))

(defn m2 [pchs durs]
  (let [p (next-in-atom pchs)
        d (next-in-atom durs)
        wet-dry 0.1] 
    (add-wet-dry wet-dry (synth2 d p))
    (cause m2 (next-beat d) (!r! pchs) (!r! durs))))


(defn m3 []
  (let [n (beat-mod (sub-beat 4) 16)
        pat #{0 2 6}
        wet-dry 0.2]
    (when (pat n)
      (add-wet-dry wet-dry (synth1 1/4 (of-range (/ (inc n) 16.0) 600 700)))))
  (cause m3 (next-beat 1/4)))

(defn m4 []
  (let [n (beat-mod (sub-beat 4) 16)
        pat #{0 1 2 3 4 5 6 7}
        wet-dry 0.3]
    (when (pat n)
      (add-wet-dry wet-dry (synth2 1/2 80))))
  (cause m4 (next-beat 1/4)))


(defn m5-freq [] (sym->freq (rand-nth '(fs5 f4 g6) )))
(defn m5 []
  (let [n (beat-mod (sub-beat 4) 16)
        pat #{0 1 2 3 4 5 6 7}
        wet-dry 0.1]
    (when (pat n)
      (add-wet-dry wet-dry (synth2 1/2 (m5-freq)))))
  (cause m5 (next-beat 1/4)))

(def m6-pchs 
  (atom 
    (cycle (concat 
      (repeat 16 160)
      (repeat 16 200)
      (repeat 16 300)
      (repeat 16 400)
      ))))

(defn m6-freq []
  (next-in-atom m6-pchs))

(defn m6 []
  (add-wet-dry 0.05 (synth1 (beats 1/4) (m6-freq)))
  (cause m6 (next-beat 1/4)))


(def m7-notes (atom nil))
(reset!! 
       m7-notes
       (cycle (lc! '(c2:2 r c 2 r g c3 g2 c r>16))))

(defn m7 []
  (let [[start dur freq] (next-in-atom m7-notes)]
    (when dur
      (when freq
        (add-wet-dry 0.1 (synth2 (beats dur) freq))) 
      (cause m7 (beat-advance 1/4 dur)))))

(def sound0 
 (let [b (create-buffer)]
   (fn [] b)))

(defn sound [] (sound0))

(defn kdrum [freq]
  (let [e (exp-env [0 20000 0.05 freq 1 freq])] 
    (-> (white-noise)
        (k35-hpf 1000 7)
        (k35-lpf e 9.8)
        (distort 8)
        (mul (xar 0.01 1) 1.0)
        (pan 0.0))))

(defn kdrum-perf [freq]
  (add-wet-dry 0.07 (kdrum freq)))

(defn play-set
  [beat pat f & args]
  (when (pat beat)
    (apply f args)))

(defn kdrum-play [] 
  (let [beat (beat-mod (sub-beat 4) 16)
        cym #{4 12 14}
        bd #{0 1 2 3 4 8 12 13}
        pat1 #{2 7 10}
        pat2 #{3 6 11}]
    (play-set beat cym kdrum-perf 10000)
    (play-set beat pat1 kdrum-perf (hertz 'c5))
    (play-set beat pat2 kdrum-perf (hertz 'fs5))
    (play-set beat bd kdrum-perf 20))
  (cause kdrum-play (next-beat 1/4)))

#_(cause kdrum-play (next-beat 4 ))

(defn pat->set
  [pat]
  (second
    (reduce 
      (fn [[indx coll] b]
        (let [c (if (= 1 b)
                  (conj coll indx)
                  coll)]
          (vector (inc indx) c)))      
      [0 #{}] 
      pat)))

(defn bass-play
  []
  (let [n (beat-mod (sub-beat 4) 16)
        pat (pat->set (euclid 16 16)) 
        freqs (map hertz '(g1 cs2 g2 cs3 g3))
        wet-dry 0.1]
    (when (pat n)
      (add-wet-dry wet-dry (bass 1/2 (rand-nth freqs)))))
  (cause bass-play (next-beat 1/4)))

#_(cause bass-play (next-beat 4 ))

(comment


  (start-engine)

  (set-tempo 106)

  ;; eval to get drums going
  (cause drums (next-beat 4))

  (def snare-pat 
    (pat->set (euclid 9 16)))


  ;; eval to get melodic line going
  ;; eval multiple times to get parallel melodic lines 
  (cause m1 (next-beat 4) 0)

  ;; can mutate the pattern sequences while m2 is running in its event stream
  (def m2-pchs (atom nil))
  (reset!! m2-pchs (cycle [60 120 60]))

  (def m2-durs (atom nil))
  (reset!! m2-durs (repeatedly #(rand-nth [1/2 1 1/2])))
  
  (cause m2 (next-beat 4) (!r! m2-pchs) (!r! m2-durs))

  ;; sequence ahead 64-beats of changes
  (let [t (next-beat 16)]
    (cause (fn [] (reset!! m2-durs (repeatedly #(rand-nth [1/2 1]))))
           t)
    (cause (fn [] (reset!! m2-durs (repeatedly #(rand-nth [1/2 1/4]))))
           (+ t 16))
    (cause (fn [] (reset!! m2-durs (repeatedly #(rand-nth [1/2 1]))))
           (+ t 32))
    (cause (fn [] (reset!! m2-durs (repeatedly #(rand-nth [1/2 1/4]))))
           (+ t 48))
    (cause (fn [] (end-recur! m2)) 
           (+ t 64)))

  (cause m3 (next-beat 4))
  (cause m4 (next-beat 4))
  (cause m5 (next-beat 4))
  (redef! m5-freq 
          (fn [] 
            (if (> (Math/random) 0.85)
              (* 80 4) 80)))

  ;; modify m5-freq to yield different values
  (redef! m5-freq 
          (let [pat (atom (cycle [80 90 100 200]))]
          (fn [] 
            (next-in-atom pat))))

  ;; schedule the function change for m5-freq
  (cause 
       #(redef! m5-freq 
          (let [pat (atom (cycle [80 90 100 200]))]
          (fn [] 
            (next-in-atom pat))))
       (next-beat 4))

  ;; eval to show beat/bar structure in REPL
  (cause beat-printer (next-beat 4) 4 16)

  (end-recur! drums)
  (end-recur! m2)

  (cause m6 (next-beat 16))
  (cause m7 (next-beat 4))

  (add-afunc 
    (with-duration (beats 8) 
      (let [e (shared (adsr (beats 8) 0.0 1.0 (beats 8)))
             pch (sym->freq 'a4)] 
        (->
          (sum (blit-triangle pch) 
               (blit-triangle (* pch 2)))
          (zdf-ladder (sum pch (mul e 4000)) 0.25)
          (mul 0.25 e )
          (pan 0.1)
          ))))

  (cause reset!! (next-beat 16) 
       (!r! m7-notes)
       (cycle 
         (concat 
           (repeat-seq 3 (lc! '(c2:2 r c 2 r g c3 g2 c r>16)))
             (lc! '(c2:2 r c 2 r g c3 g2 c c3)))))

  (cause reset!! (next-beat 16)
         (!r! m7-notes)
         (cycle (lc! '(c2:2 r c r c r>8
                       cs:2 r cs r cs r>16))))


  (cause 
    #(redef! sound0
       (-> (blit-saw 80
             #_(exp-env [0 60 (beats 32) 6000]))
           (zdf-ladder (sum 800 (lfo 1600 (/ 1.0 (beats 1/4)) :saw)) 0.5)
           (pan 0.0)))
    (next-beat 4))

  (cause add-afunc (next-beat 4) sound)


  (stop-engine)
  )

