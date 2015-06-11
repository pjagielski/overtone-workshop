(ns overtone-workshop.letsgo
  (:use [overtone.live])
  (:require [overtone-workshop.player :refer :all]
            [overtone-workshop.patterns :refer :all]
            [overtone-workshop.sounds :refer :all]))

(definst lead [note 60 release 0.3 amp 0.4 sub-gate 0.15]
  (let [freq  (midicps note)
        freq2 (midicps (+ note 0.08))
        freq3 (midicps (+ note 0.16))
        freq4 (midicps (+ note 0.24))
        freq5 (midicps (- note 0.12))
        osc   (saw [freq freq2 freq3 freq4 freq5])
        osc1  (lf-tri freq)
        sub   (lpf (pulse (* freq 0.5) 0.3) 500)
        osc   (+ (* sub-gate sub) (* 0.05 osc1) (* amp osc))
        mix   (mix osc)
        env   (env-gen (env-lin 0.015 0.20 release) :action FREE)]
    (pan2 (* mix env))))

(comment (lead :note 70))
(def brvb (inst-fx! lead fx-freeverb))
(ctl brvb :room-size 0.8 :wet-dry 0.5 :dampening 0.25)

(definst bass [note 60 amp 0.5 osc-mix 0.2 cutoff 0.35 sustain 0.2 release 0.15 fil-dec 0.85 fil-amt 1500]
  (let [freq (midicps note)
        sub-freq (midicps (- note 12))
        osc1 (saw:ar freq)
        osc2 (pulse sub-freq 0.5)
        osc (+ (* osc-mix osc2) (* (- 1 osc-mix) osc1))
        snd [osc osc]
        fil-env (env-gen (adsr 0.0 fil-dec 0.1 fil-dec))
        snd (lpf snd (+ (* fil-env fil-amt) (lin-exp cutoff 0.0 1.0 20.0 20000.0)))
        env (env-gen (env-lin 0.01 sustain release) 1 1 0 1 FREE)]
    (out 0 (* amp env snd))))

(comment (bass :note 50))

(definst bend-noise [amp 0.7 decay 0.85 cutoff 0.65]
  (let [osc (white-noise)
        snd [osc osc]
        snd (bpf snd (lin-exp cutoff 0.0 1.0 20.0 20000.0) 0.5)
        env (env-gen (env-adsr 0.0 decay 0.0 0.7) :action FREE)]
  (out 0 (* amp env snd))))

(comment (bend-noise))

(defn noise-player [nome beat]
  (let [next-beat (+ 4 beat)]
    (at (nome (+ 3 beat)) (bend-noise))
    (apply-by (nome next-beat) noise-player [nome next-beat])))

(def kick (sample "resources/kick.wav"))
(def clap (sample "resources/clap.wav"))
(def tambo (sample "resources/tambo.wav"))

(defn beat-player [nome beat]
  (let [next-beat (+ 2 beat)]
    (at (nome beat) (kick))
    (at (nome (+ 0.5 beat)) (tambo))
    (at (nome (+ 1 beat)) (do (kick) (clap)))
    (at (nome (+ 1.5 beat)) (tambo))
    (apply-by (nome next-beat) beat-player [nome next-beat])))

(defn play-bass [step-ctl]
  (if-let [sustain (get step-ctl :sustain)]
    (partial bass :sustain sustain)
    (partial bass)))

(defn play-all [nome]
  (let [beat (nome)]
      (beat-player nome beat)
      (noise-player nome beat)
      (player letsgo {} nome beat #'lead 16 64)
      (player letsgo-bass letsgo-bass-ctrl nome beat #'bass 16 64)))

(comment
  (let [nome (metronome 128)]
    (play-all nome)))

(stop)
