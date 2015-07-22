(ns overtone-workshop.fx
  (:use [overtone.live])
  (:require [overtone-workshop.player :refer :all]
            [overtone-workshop.patterns :refer :all]
            [overtone.inst.synth :refer [ks1-demo]]))

(defsynth fx-chorus2 [bus 0 gate 1]
  (let [source (in:ar bus 2)
        env (linen:kr gate 0.1 0.5 4 2)
        snd-fn (fn []
                 (let [max-delay (ranged-rand 0.005 0.02)]
                   (delay-c:ar source
                               max-delay
                               (+ (* 0.75 max-delay)
                                  (* (* 0.25 max-delay)
                                     (lf-noise1:kr (ranged-rand 4.5 10.5)))))))
        snd (mix (repeatedly 17 snd-fn))]
    (replace-out bus
                 (pan2 (+ (* 0.2 source) (* 8 env snd))))))

(definst gold-strings [freq 440 bus 0 lfo-rate 5900 lfo-width 0.01 rq 0.5 sustain 0.4]
  (let [lfo (lf-tri:ar lfo-rate (ranged-rand 0 2.0))
        snd (saw:ar (* freq (+ (* lfo-width lfo)
                               1.0)))
        flt (b-hi-pass snd freq rq)
        env (env-gen (env-lin 0.2 sustain 0.7))]
    (pan2 (* flt env (* 0.5 snd)))))

(comment
  (gold-strings :freq (midi->hz 72) :sustain 0.5)
  (gold-strings :freq (midi->hz 68) :sustain 1.5)
  (gold-strings :freq (midi->hz 61) :sustain 1.5)
  (inst-fx! gold-strings fx-chorus2)
  (clear-fx gold-strings))

(def nome (metronome 128))

(def play-ks1 (partial ks1-demo))

(defsynth fx-chorus2 [bus 0]
  (let [source (in bus)
        snd (delay-c:ar source
                        (ranged-rand 0.005 0.02) (lf-noise1:kr (i-rand 4.5 10.5)))]
    (replace-out bus snd)))

(comment
  (player derezed {} nome (nome) play-ks1 8 32)
  (inst-fx! ks1-demo fx-distortion)
  (inst-fx! ks1-demo fx-echo)
  (clear-fx ks1-demo)
  (stop))

(def count-down (sample (freesound-path 71128)))

(comment
  (count-down)
  (stop))

;; From Designing Sound in SuperCollider
(defsynth schroeder-reverb-countdown [rate 1]
  (let [input (pan2 (play-buf 1 count-down rate :action FREE) -0.5)
        delrd (local-in 4)
        output (+ input [(first delrd) (second delrd)])
        sig [(+ (first output) (second output)) (- (first output) (second output))
             (+ (nth delrd 2) (nth delrd 3)) (- (nth delrd 2) (nth delrd 3))]
        sig [(+ (nth sig 0) (nth sig 2)) (+ (nth sig 1) (nth sig 3))
             (- (nth sig 0) (nth sig 2)) (- (nth sig 0) (nth sig 2))]
        sig (* sig [0.4])
        deltimes (- (* [101 143 165 177] 0.001) (control-dur))]
    (local-out (delay-c sig deltimes deltimes))
    (out 0 output)))

(comment
  (schroeder-reverb-countdown :rate 0.8)
  (schroeder-reverb-countdown :rate 2)
  (stop))

