(ns overtone-workshop.fx
  (:use [overtone.live])
  (:require [overtone-workshop.player :refer :all]
            [overtone-workshop.patterns :refer :all]
            [overtone.inst.synth :refer [ks1-demo]]))

(def nome (metronome 128))

(defn play-ks1 [_]
  (partial ks1-demo))

(comment
  (player derezed {} nome (nome) play-ks1 8 32)
  (inst-fx! ks1-demo fx-distortion)
  (inst-fx! ks1-demo fx-echo)
  (clear-fx ks1-demo)
  (ks1-demo 60)
  (ks1-demo 40)
  (stop))

(def count-down (sample (freesound-path 71128)))

(comment
  (count-down)
  (stop))

;; From Designing Sound in SuperCollider
(defsynth schroeder-reverb-countdown
  [rate 1]
  (let [input    (pan2 (play-buf 1 count-down rate :action FREE) -0.5)
        delrd    (local-in 4)
        output   (+ input [(first delrd) (second delrd)])
        sig      [(+ (first output) (second output)) (- (first output) (second output))
                  (+ (nth delrd 2) (nth delrd 3)) (- (nth delrd 2) (nth delrd 3))]
        sig      [(+ (nth sig 0) (nth sig 2)) (+ (nth sig 1) (nth sig 3))
                  (- (nth sig 0) (nth sig 2)) (- (nth sig 0) (nth sig 2))]
        sig      (* sig [0.4 0.37 0.333 0.3])
        deltimes (- (* [101 143 165 177] 0.001) (control-dur))
        lout     (local-out (delay-c sig deltimes deltimes))
        ]
    (out 0 output)))

(comment
  (schroeder-reverb-countdown :rate 0.8)
  (schroeder-reverb-countdown :rate 0.99)
  (stop))

