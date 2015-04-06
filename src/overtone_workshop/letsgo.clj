(ns overtone-workshop.letsgo
  (:use [overtone.live])
  (:require [overtone-workshop.player :refer :all]
            [overtone-workshop.lead :refer :all]
            [overtone-workshop.patterns :refer :all]))

(definst bass [note 60 amp 0.7 osc-mix 0.3 cutoff 0.43 sustain 0.2 release 0.15 fil-dec 0.7 fil-amt 1500]
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

(definst bend-noise [amp 0.5 decay 0.85 cutoff 0.65]
  (let [osc (white-noise)
        snd [osc osc]
        snd (bpf snd (lin-exp cutoff 0.0 1.0 20.0 20000.0) 0.5)
        env (env-gen (env-adsr 0.0 decay 0.0 0.7) :action FREE)]
  (out 0 (* amp env snd))))

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

(comment
  (def nome (metronome 128))
  (let [beat (nome)]
    (beat-player nome beat)
    (noise-player nome beat)
    (player letsgo {} nome beat play-lead 16 64)
    (player letsgo-bass letsgo-bass-ctrl nome beat play-bass 16 64))
  (stop))

