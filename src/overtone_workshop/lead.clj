(ns overtone-workshop.lead
  (:use [overtone.live])
  (:require [overtone-workshop.patterns :refer :all]
            [overtone-workshop.player :refer :all]))

(definst lead [note 60 release 0.30 gate 0.4 sub-gate 0.3]
  (let [freq  (midicps note)
        freq2 (midicps (+ note 0.08))
        freq3 (midicps (+ note 0.20))
        freq4 (midicps (+ note 0.48))
        osc   (saw:ar [freq freq2 freq3 freq4])
        sub   (lpf (saw (* freq 0.5)) 1000)
        osc   (+ (* sub-gate sub) (* gate osc))
        mix   (mix osc 1.0 0.3)
        env   (env-gen (env-lin 0.015 0.20 release) :action FREE)]
    (pan2:ar (* mix env))))

(comment
  (lead :sub-gate 0.5)
  (def rv (inst-fx! lead fx-freeverb))
  (def ch (inst-fx! lead fx-chorus))
  (def eh (inst-fx! lead fx-echo))
  (inst-fx! lead fx-distortion2)
  (clear-fx lead)
  (ctl rv :room-size 0.7 :wet-dry 0.2)
  (ctl ch :rate 0.02)
  (ctl eh :max-delay 0.1 :decay-time 0.1)
  (stop))

(comment
  (on-event [:midi :note-on]
    (fn [{note :note}] (lead :note note))
    ::handler)
  (remove-event-handler ::handler))

(defn play-lead [controls]
  (partial lead))

(def lead-nome (metronome 128))

(defn lead-player [pattern beats resolution]
  (simple-player {:pattern pattern :nome lead-nome :beat (lead-nome) :synth-fn play-lead :beats beats :resolution resolution}))

(comment
  (lead-player letsgo 16 64))

(comment
  (lead-player animals 16 64))

(comment
  (lead-player ratherbe 32 128))

(stop)

