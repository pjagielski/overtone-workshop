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
  (demo 2 (saw 300))
  (demo 2 (saw 200))
  (demo 2 (pink-noise)))

;; stereo
(comment
  (demo 2 (pan2 (saw 200))))

;; multi-channel expansion
(comment
  (demo 2 (saw [199 200]))
  (demo 2 (saw [199 200 201]))
  (demo 2 (pan2 (mix (pulse [199 200]))))
  (demo 2 (pan2 (mix (saw [199 200])))) 
  (demo 2 (pan2 (mix (saw [99 100 101])))))

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
  (ctl s :note (note :C3))
  (ctl s :note (note :B#4))
  (ctl s :note (note :C5))
  (ctl s :note (note :C2))
  (stop))

;; filters
(comment
  (demo 10 (pan2 (mix (lpf (saw [99 100 101]) (mouse-x 40 5000 EXP)))))
  (demo 10 (pan2 (mix (hpf (saw [99 100 101]) (mouse-x 40 5000 EXP)))))
  (demo 15 (pan2 (mix (bpf (saw [99 100 101]) (mouse-x 40 5000 EXP) (mouse-y 0.01 1 LIN)))))
  (demo 15 (pan2 (mix (rlpf (saw [99 100 101]) (mouse-x 40 5000 EXP) (mouse-y 0.01 1 LIN)))))
  (stop))

(defcgen wobble [src wobble-factor]
  (:ar
   (let [sweep (lin-exp (lf-tri wobble-factor) -1 1 40 2000)
         wob   (lpf src sweep)
         wob   (* 0.8 (normalizer wob))
         wob   (+ wob (bpf wob 1500 2))]
     (+ wob (* 0.2 (g-verb wob 9 0.7 0.7))))))

(comment
  (demo 3 (wobble (saw [99 100 101]) 3))
  (demo 3 (wobble (mix (saw [99 100 101])) 3)))

;; detuning
(definst multi-osc [note 60 osc2-semi 0 amp 0.3]
  (let [freq  (midicps note)
        osc1  (pulse freq)
        freq2 (midicps (+ note osc2-semi))
        osc2  (saw freq2)
        snd   (+ osc1 osc2)]
    (pan2 (* amp snd))))

(note :C4)
(note :C5)

(comment
  (def m (multi-osc))
  (ctl m :osc2-semi 12)
  (ctl m :osc2-semi 24)
  (ctl m :osc2-semi 7)
  (ctl m :osc2-semi 7.08)
  (ctl m :osc2-semi 7.16)
  (ctl m :osc2-semi 11.96)
  (ctl m :osc2-semi 0.08)
  (ctl m :osc2-semi 0.60)
  (ctl m :osc2-semi 0.80)
  (stop))

(midi->hz 44)
(midi->hz 44.16)
(note :G#2)

(comment
  (demo 3 (wobble (saw [102 103 104]) 3)) 
  (demo 3 (wobble (saw [99 103 107]) 3)))

;; envelope
(definst my-saw [note 60]
  (pan2 (saw (midicps note))))

(comment
  (my-saw)
  (stop))

(definst my-env-saw [note 60 attack 0.01 sustain 0.4 release 0.1] 
  (* (env-gen (env-lin attack sustain release))
     (saw (midicps note))))

(comment
  (my-env-saw)
  (my-env-saw :sustain 1.0) 
  (my-env-saw :attack 0.5) 
  (my-env-saw :attack 0.5 :release 0.5))

;; chords
(defn play-chord [a-chord inst]
  (doseq [note a-chord] (inst :note note)))

(comment
  (play-chord (chord :C4 :major) my-env-saw)
  (play-chord (chord :G3 :major) my-env-saw)
  (play-chord (chord :F3 :sus4) my-env-saw)
  (play-chord (chord :C4 :major) (partial my-env-saw :sustain 1.0))
  (play-chord (map note [:G#5 :C#5 :F4]) my-env-saw))

(definst my-lead [note 60 attack 0.01 sustain 0.4 release 0.1]
  (let [freqs [(midicps note) (midicps (+ note 0.08))]]
    (* (env-gen (env-lin attack sustain release))
       (saw freqs))))

(comment
  (play-chord (chord :C4 :major) my-lead)
  (play-chord (chord :G3 :major) my-lead)
  (play-chord (chord :F3 :sus4) my-lead)
  (play-chord (chord :C4 :7sus4) my-lead)
  (play-chord (chord :C4 :m13) my-lead)
  (play-chord (chord :C3 :m11) my-lead)
  (play-chord (map note [:G#5 :C#5 :F4]) my-lead) 
  (play-chord (map note [:B6 :G6 :B5 :G5 :B4]) my-lead))

