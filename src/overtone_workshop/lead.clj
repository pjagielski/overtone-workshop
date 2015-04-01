(ns overtone-workshop.lead
  (:use [overtone.live])
  (:require [overtone-workshop.patterns :refer :all]
            [overtone-workshop.player :refer :all]))

(definst lead [note 60 release 0.25 gate 0.4 sub-gate 0.0]
  (let [freq  (midicps note)
        freq2 (midicps (+ note 0.08))
        freq3 (midicps (+ note 0.20))
        freq4 (midicps (+ note 0.48))
        osc   (saw:ar [freq freq2 freq3 freq4])
        sub   (lpf (saw (* freq 0.5)) 1000)
        osc   (+ (* sub-gate sub) (* gate osc))
        mix   (mix osc 1.0 0.3)
        env   (env-gen (env-lin 0.015 0.2 release) :action FREE)]
    (pan2:ar (* mix env))))

(comment
  (lead)
  (def rv (inst-fx! lead fx-freeverb))
  (def ch (inst-fx! lead fx-chorus))
  (def eh (inst-fx! lead fx-echo))
  (inst-fx! lead fx-distortion2)
  (clear-fx lead)
  (ctl rv :room-size 0.7 :wet-dry 0.4)
  (ctl ch :rate 0.02)
  (ctl eh :max-delay 0.1 :decay-time 0.1)
  (stop))

(comment
  (on-event [:midi :note-on]
    (fn [{note :note}] (lead :note note))
    ::handler)
  (remove-event-handler ::handler))

(defn play-synth [controls]
  (partial lead))

(def nome (metronome 128))

(defn lead-player [pattern]
  (simple-player {:pattern pattern :nome nome :beat (nome) :synth-fn play-synth}))

(comment
  (lead-player letsgo))

(comment
  (lead-player animals))

(comment
  (lead-player ratherbe))

(stop)
