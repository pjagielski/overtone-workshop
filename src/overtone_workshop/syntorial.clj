(ns overtone-workshop.syntorial
  (:use [overtone.live])
  (:require [overtone-workshop.player :refer :all]
            [overtone-workshop.lead :refer :all]  
            [overtone-workshop.patterns :refer :all]))

(def wave {:saw 0 :pulse 1})
(def semitone {:fifth 7, :octave 12, :octave-and-fifth 19, :two-octaves 24,
               :two-octaves-and-fifth 31, :three-octaves 36})

(definst syntorial-synth
  [note 60 amp 1.0 osc1-waveform 0 osc1-pulsewidth 0.50 osc1-semi 0
   osc2-waveform 0 osc2-pulsewidth 0.50 osc2-semi 0 osc-mix 0.00
   cutoff 1.0 attack 0.015 sustain 0.2 release 0.015 dist 0.9 master-volume 0.0]
  (let [note1-freq  (midicps (+ note osc1-semi))
        note2-freq  (midicps (+ note osc2-semi))
        osc1-saw    (saw:ar note1-freq)
        osc1-pulse  (pulse note1-freq osc1-pulsewidth)
        osc1        (select osc1-waveform [osc1-saw osc1-pulse])
        osc2-saw    (lf-saw note2-freq)
        osc2-pulse  (pulse note2-freq osc2-pulsewidth)
        osc2        (select osc2-waveform [osc2-saw osc2-pulse])
        osc         (+ (* osc-mix osc2) (* (- 1 osc-mix) osc1))
        k           (/ (* 2 dist) (- 1 dist))
        osc         (/ (* osc (+ 1 k)) (+ 1 (* k (abs osc))))
        osc-stereo  [osc osc]
        filt-stereo (lpf osc-stereo (lin-exp cutoff 0.0 1.0 20.0 20000.0))
        env         (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
        master      (pow 10 (/ master-volume 20))]
    (out 0 (* amp master env filt-stereo))))

(defn partial-syntorial [controls]
  (partial syntorial-synth
     :osc1-waveform   (:osc1-waveform controls)
     :osc1-pulsewidth (:osc1-pulsewidth controls)
     :osc1-semi       (:osc1-semi controls)
     :osc2-waveform   (:osc2-waveform controls)
     :osc2-pulsewidth (:osc2-pulsewidth controls)
     :osc2-semi       (:osc2-semi controls)
     :osc-mix         (:osc-mix controls)
     :cutoff          (:cutoff controls)
     :amp             (:amp controls)
     :master-volume   (:master-volume controls)
     :attack          (:attack controls)
     :sustain         (:sustain controls)
     :release         (:release controls)))

(defn untztrument [instr synth-controls controls]
  (on-event [:midi :control-change]
             (fn [{value :velocity note :note}]
                (when-let [control (get @controls note)]
                  (let [normalized-value (/ (- value 1) 127)
                        scaled-value (+ (:min control) (* normalized-value (- (:max control) (:min control))))]
                    (swap! synth-controls assoc (:param control) scaled-value))))
              ::untztrument-control)
  (on-event [:midi :note-on]
            (fn [{note :note}] (apply (instr synth-controls) [:note note]))
              ::untztrument-note))

(def syntorial-controls
  (atom {:osc1-waveform (:pulse wave) :osc1-pulsewidth 0.25 :osc1-semi 0
         :osc2-waveform (:pulse wave) :osc2-pulsewidth 0.25 :osc2-semi 0
         :osc-mix 0.0 :cutoff 0.65 :master-volume 4.0 :amp 1.0
         :attack 0.01 :sustain 0.2 :release 0.2}))

(def controls (atom {13 {:param :cutoff :min 0.0 :max 1.0}
                     11 {:param :osc-mix :min 0.0 :max 1.0}
                      1 {:param :amp :min 0.1 :max 1.0}}))

(def nome (metronome 128))

(defn play-syntorial [step-ctrl]
  (partial-syntorial (merge @syntorial-controls step-ctrl)))

(comment
  (untztrument partial-syntorial syntorial-controls controls)
  (untztrument partial-lead lead-controls controls)
  (remove-event-handler ::untztrument-control)
  (remove-event-handler ::untztrument-note)
  (player ratherbe {} nome (nome) partial-syntorial syntorial-controls 32 128)
  (swap! syntorial-controls assoc :osc2-semi -12, :master-volume 4.0, :osc1-waveform 0, :osc-mix 0.5, :release 0.1, :amp 0.3, :osc1-semi 0, :osc2-pulsewidth 0.95, :sustain 0.2, :osc2-waveform 1, :attack 0.01, :cutoff 0.65, :osc1-pulsewidth 0.95)
  (defn next-beat [nome]
    (+ (* 16 (int (/ (nome) 16))) 16))
  (player letsgo {} nome (next-beat nome) play-lead 16 64)
  (player letsgo-bass letsgo-bass-ctrl nome (next-beat nome) play-syntorial 16 64)
  (stop)
)

