(ns overtone-workshop.midi
  (:use [overtone.live]
        [overtone.inst.sampled-piano])
  (:require [overtone-workshop.player :refer :all]
            [overtone-workshop.sounds :refer [my-lead play-chord]]
            [overtone-workshop.patterns :refer :all]
            [overtone-workshop.timing :refer [kick snare hat dubstep]]
            [overtone-workshop.letsgo :refer [bass]]
            [overtone-workshop.untztrument :refer :all]))

(def dp {60 (sample "resources/harder.wav")
         61 (sample "resources/better.wav")
         62 (sample "resources/faster.wav")
         63 (sample "resources/stronger.wav")
         64 (sample "resources/work_it.wav")
         65 (sample "resources/make_it.wav")
         66 (sample "resources/do_it.wav")
         67 (sample "resources/makes_us.wav")})

(defsynth tone [freq 40 nharm 12 detune 0.2 gate 0 pan 0 amp 1 out-bus 0]
  (let [env (env-gen (adsr 0.05 0.1 0.5 0.5) gate)
        sig (blip:ar (* freq (midiratio (* detune (lf-noise1 (repeat 16 0.2))))) nharm)
       ; sig (* sig (lin-exp (lf-noise1 (repeat 16 0.5)) 0.1 1 0.1 1))
        sig (splay:ar sig)
        sig (balance2:ar (nth sig 0) (nth sig 1) pan)
        sig (* sig env 20 amp)]
    (out:ar out-bus sig)))

(comment
  (def x (tone :gate 1))
  (def controls {13 {:control :nharm  :min 12  :max 40}
                 11 {:control :freq   :min 40  :max 440}
                  1 {:control :detune :min 0.1 :max 20}})

  (on-event [:midi :control-change]
    (fn [{value :velocity note :note}]
      (when-let [control (get controls note)]
        (ctl x (:control control) (prescale value (:min control) (:max control)))))
    ::midi-player)
  (remove-event-handler ::midi-player)
  (ctl x :gate 0))

(comment
  (midi-connected-devices)

  (on-event [:midi :note-on]
    (fn [{note :note}] (my-lead note))
    ::midi-player)
  (remove-event-handler ::midi-player)

  (def chords {60 [:B6 :G6 :B5 :G5 :B4]
               61 [:E7 :B6 :G6 :E6 :B5 :G5 :B4]
               62 [:C7 :E6 :C6 :G5 :C5]
               63 [:F#7 :D7 :F#6 :D6 :A5 :D5]
               67 [:G7 :E7 :G6 :E6 :B5 :E5]})
  (def l (partial my-lead :sustain 0.2))
  (on-event [:midi :note-on]
    (fn [{n :note}] (play-chord (map note (get chords n)) l))
    ::midi-player)
  (remove-event-handler ::midi-player)

  (def drums {60 kick 61 snare 62 hat})
  (on-event [:midi :note-on]
    (fn [{n :note}] (apply (get drums n) []))
    ::midi-player)
  (remove-event-handler ::midi-player)

  (on-event [:midi :note-on]
    (fn [{n :note}] (apply (get dp n) []))
    ::midi-player)
  (remove-event-handler ::midi-player) )

(def synth-controls (atom {}))
(#_ (swap! synth-controls assoc :cutoff 0.43 :fil-amt 1000 :fil-dec 0.5))
(swap! synth-controls assoc :cutoff 0.01 :fil-amt 3500 :fil-dec 1.5)

(comment
  (bass)
  (def bb (partial bass :note 50))
  (bb)
  (def lb #(apply bass [:cutoff 0.6 :note 40]))
  (lb)
  (def rd #(apply bass (to-vec {:cutoff 0.6 :note 40})))
  (rd))

(def *controls (atom {13 {:param :cutoff :min 0.0 :max 1.0}
                      11 {:param :fil-amt :min 0.0 :max 4500}
                       1 {:param :fil-dec :min 0.1 :max 1.5}}))

(def daft-kick (freesound-sample 177908))
(def clap (freesound-sample 196250))
(def fat-kick (sample "resources/fat_kick.aif"))

(def play-bass
  (partial play-with-controls #'bass synth-controls))

(comment
  (def d (dubstep))
  (def wobble {60 2 61 4 62 8})
  (on-event [:midi :note-on]
    (fn [{note :note}]
      (ctl d :note (- note 24))
      (ctl d :wobble (mod (* (rand 4) note) 8)))
    ::midi-player)
  (remove-event-handler ::midi-player)
  (stop))

(comment
  (untztrument play-bass synth-controls *controls)
  (remove-event-handler ::untztrument-note)
  (remove-event-handler ::untztrument-control)
  (let [nome (metronome 128) beat (nome)]
    (player letsgo-bass {} nome beat play-bass 16 64))
  (stop))
