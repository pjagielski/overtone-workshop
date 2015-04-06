(ns overtone-workshop.midi
  (:use [overtone.live])
  (:require [overtone-workshop.player :refer :all]
            [overtone-workshop.patterns :refer :all]
            [overtone-workshop.letsgo :refer [bass]]))

(def synth-controls (atom {}))
(#_ (swap! synth-controls assoc :cutoff 0.43 :fil-amt 1000 :fil-dec 0.5))
(swap! synth-controls assoc :cutoff 0.53 :fil-amt 0 :fil-dec 0)

(defn play-bass [step-ctl]
  (let [partial-fn (partial bass
                 :fil-amt (get @synth-controls :fil-amt)
                 :fil-dec (get @synth-controls :fil-dec)
                 :cutoff (get @synth-controls :cutoff))]
  (if-let [sustain (get step-ctl :sustain)]
    (partial partial-fn :sustain sustain)
    partial-fn)))

(defn untztrument [play-fn synth-controls controls]
  (on-event [:midi :control-change]
             (fn [{value :velocity note :note}]
                (when-let [control (get @controls note)]
                  (let [normalized-value (/ (- value 1) 127)
                        scaled-value (+ (:min control) (* normalized-value (- (:max control) (:min control))))]
                    (swap! synth-controls assoc (:param control) scaled-value))))
              ::untztrument-control)
  (on-event [:midi :note-on]
            (fn [{note :note}] (apply (play-fn {}) [:note note]))
              ::untztrument-note))

(def controls (atom {13 {:param :cutoff :min 0.0 :max 1.0}
                     11 {:param :fil-amt :min 0.0 :max 1500}
                      1 {:param :fil-dec :min 0.1 :max 1.5}}))

(def nome (metronome 128))

(comment
  (untztrument play-bass synth-controls controls)
  (let [beat (nome)]
    (player letsgo-bass letsgo-bass-ctrl nome beat play-bass 16 64))
  (stop))

