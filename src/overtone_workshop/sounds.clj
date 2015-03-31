(ns overtone-workshop.sounds
  (use [overtone.live]))

;; oscilators
(comment
  (demo 2 (sin-osc 440))
  (demo 2 (saw 440))
  (demo 2 (pulse 440))
  (demo 2 (pulse 440 0.95))
  (demo 2 (pulse 440 0.15))
  (demo 2 (lf-tri 440))
  (demo 2 (saw 200))
  (demo 2 (pink-noise)))

;; stereo
(comment
  (demo 2 (pan2 (saw 200))))

;; multi-channel expansion
(comment 
  (demo 2 (saw [199 200]))
  (demo 2 (saw [199 200 201]))
  (demo 2 (pan2 (mix (pulse [199 200 201]))))
  (demo 2 (pan2 (mix (saw [199 200 201])))))

;; notes
(note :C4)
(midi->hz (note :C4))

;; synth
(defsynth my-saw [note 60]
  (let [freq (midicps note)
        freqs [(+ 1 freq) freq (- 1 freq)]]
      (out 0 (pan2 (mix (saw freqs))))))

(comment 
  (def s (my-saw))
  (ctl s :note (note :C5))
  (ctl s :note (note :B#4))
  (stop))

;; filters
(comment
  (demo 10 (pan2 (mix (lpf (saw [99 100 101]) (mouse-x 40 5000 EXP)))))
  (demo 10 (pan2 (mix (hpf (saw [99 100 101]) (mouse-x 40 5000 EXP)))))
  (demo 30 (pan2 (mix (bpf (saw [99 100 101]) (mouse-x 40 5000 EXP) (mouse-y 0.01 1 LIN)))))
  (stop))

(defcgen wobble [src wobble-factor]
  (:ar
   (let [sweep (lin-exp (lf-tri wobble-factor) -1 1 40 3000)
         wob   (lpf src sweep)
         wob   (* 0.8 (normalizer wob))
         wob   (+ wob (bpf wob 1500 2))]
     (+ wob (* 0.2 (g-verb wob 9 0.7 0.7))))))

(comment 
  (demo 3 (wobble (saw [99 100 101]) 3))
  (demo 3 (wobble (mix (saw [99 100 101])) 3)))

(definst multi-osc [note 60 osc2-semi 0 amp 0.3]
  (let [freq  (midicps note)
        osc1  (pulse freq)
        freq2 (midicps (+ note osc2-semi))
        osc2  (saw freq2)
        snd   (+ osc1 osc2)]
    (pan2:ar (* amp snd))))

(comment
  (def m (multi-osc))
  (ctl m :osc2-semi 7)
  (ctl m :osc2-semi 11.96)
  (inst-fx! multi-osc fx-echo)
  (def ch (inst-fx! multi-osc fx-chorus))
  (ctl ch :rate 0.02)
  (ctl ch :rate 0.2)
  (clear-fx multi-osc)   
  (stop))

