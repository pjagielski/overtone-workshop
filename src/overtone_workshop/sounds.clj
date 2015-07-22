(ns overtone-workshop.sounds
  (use [overtone.live]
       [overtone.studio.scope])
  (require [overtone.inst.synth :refer [supersaw]]))

;;   ╭-╮
;;   | | |     =>   ---╲___    =>     /\
;;     ╰-╯                           /  \
;;
;;  signal           filter        amplitude


;; signals (oscilators)
(comment
  (scope 0)
  (demo 2 (sin-osc))
  (demo 2 (saw))
  (demo 2 (pulse))
  (demo 2 (pulse :width 0.95))
  (demo 2 (pulse :width 0.15))
  (demo 2 (sin-osc 220))
  (demo 2 (saw 300))
  (demo 2 (saw 200))
  (demo 2 (white-noise))
  (demo 2 (pink-noise)))

;; stereo
(comment
  (demo 2 (pan2 (saw 200))))

;; multi-channel expansion
(comment
  (demo 2 (saw [200 200]))
  (demo 2 (saw [199 200]))
  (demo 2 (pan2 (mix (saw [199 200]))))
  (demo 2 (pan2 (mix (saw [99 100 101])))))

;; notes
(note :C4)
(midi->hz (note :c3))
(midi->hz (note :c4))

;; synth
(defsynth my-saw [note 60]
  (let [freq (midicps note)
        freqs [(- freq 1) freq (+ 1 freq)]]
      (out 0 (pan2 (mix (saw freqs))))))

(comment
  (def s (my-saw))
  (ctl s :note (note :C3))
  (ctl s :note (note :B#4))
  (ctl s :note (note :A4))
  (ctl s :note (note :C3))
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
  (demo 3 (wobble (saw [99 100]) 3))
  (demo 3 (wobble (mix (saw [99 100 101])) 3)))

;; detuning
(definst multi-osc [note 60 osc2-semi 0 amp 0.3]
  (let [freq  (midicps note)
        osc1  (saw freq)
        freq2 (midicps (+ note osc2-semi))
        osc2  (saw freq2)
        snd   (+ osc1 osc2)]
    (pan2 (* amp snd))))

(- (note :C5) (note :C4))

(comment
  (def m (multi-osc))
  (ctl m :osc2-semi 12)
  (ctl m :osc2-semi 24)
  (ctl m :osc2-semi 7)
  (ctl m :osc2-semi 0.08)
  (ctl m :osc2-semi 0.16)
  (ctl m :osc2-semi 0.60)
  (ctl m :osc2-semi 0.80)
  (stop))

(comment
  (demo 3 (wobble (saw [101 103 104]) 3))
  (demo 3 (wobble (saw [100 103 106]) 3)))

;; envelope
;;
;;  /\___
;; /     \
;; A D S R

(definst my-env-synth [note 60 attack 0.01 sustain 0.4 release 0.1]
 (* (env-gen (env-lin attack sustain release))
     (sin-osc (* 0.5 (midicps note)))))

(comment
  (my-env-synth)
  (my-env-synth :sustain 1.0)
  (my-env-synth :attack 0.5)
  (my-env-synth :attack 0.5 :release 0.5))

;; chords
(defn play-chord [a-chord inst]
  (doseq [note a-chord] (inst :note note)))

(comment
  (play-chord (chord :C4 :major) my-env-synth)
  (play-chord (chord :F4 :major) my-env-synth)
  (play-chord (chord :A3 :major) my-env-synth)
  (play-chord (chord :G3 :major) (partial my-env-synth :sustain 1.0)))

(defn detune [saws detune]
  (map-indexed
    (fn [i _] (+ (- detune) (* i 2 (/ detune (- saws 1)))))
    (repeat saws detune)))

(detune 3 0.2)

(definst my-lead [note 60 semi 0 attack 0.001 sustain 0.5 release 0.2 amp 0.4 delay 0.45 saws 3 det 0.2]
  (let [note  (+ note semi)
        freqs (map #(midicps (+ note %)) (detune 3 0.2))
        src   (pan2 (mix (saw freqs)))]
    (* amp src
       (env-gen (env-lin attack sustain release)))))

(comment
  (my-lead :note 48)
  (play-chord (chord :C4 :major) my-lead)
  (play-chord (chord :F4 :major) my-lead)
  (play-chord (chord :A3 :minor) my-lead)
  (play-chord (chord :G3 :major) my-lead))

