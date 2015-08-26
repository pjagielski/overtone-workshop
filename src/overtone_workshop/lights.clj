(ns overtone-workshop.lights
  (:use [overtone.live])
  (:require [overtone-workshop.player :refer :all]
            [overtone-workshop.patterns :refer :all]))

(definst bass [note 60 fine 0.12 cutoff 0.52 amp 0.7 sustain 0.4 release 0.15]
  (let [freq (midicps note)
        osc1 (saw freq)
        snd  (mix [osc1])
      snd  (rlpf snd (lin-exp cutoff 0.0 1.0 20.0 20000.0) 0.65)
      env  (env-gen (env-lin 0.01 sustain release) 1 1 0 1 FREE)]
  (pan2 (* snd env amp))))

(comment
  (definst bass [note 60 fine 0.12 cutoff 0.62 amp 1.2 sustain 0.4 release 0.15]
    (let [freq (midicps note)
          osc1 (saw freq)
          osc2 (saw (midicps (+ note fine)))
          snd  (mix [osc1 osc2])
          snd  (rlpf snd (lin-exp cutoff 0.0 1.0 20.0 20000.0) 0.65)
          env  (env-gen (env-lin 0.01 sustain release) 1 1 0 1 FREE)]
      (pan2 (* snd env amp))))

  (definst bass [note 60 fine 0.12 cutoff 0.33 contour 0.45 sub-amp 0.5 amp 0.9 sustain 0.4 release 0.15]
    (let [freq (midicps note)
          osc1 (saw freq)
          osc2 (saw (midicps (+ note fine)))
          sub  (* sub-amp (pulse (/ freq 2)))
          snd  (mix [osc1 osc2 sub])
          fil-env (env-gen (adsr 0.1 0.75 0.1 0.2))
          snd  (rlpf snd (+ (* fil-env (* contour 10000))
                            (lin-exp cutoff 0.0 1.0 20.0 20000.0)) 0.65)
          env  (env-gen (env-lin 0.01 sustain release) 1 1 0 1 FREE)]
      (pan2 (* snd env amp)))))

(definst bouncy [note 60 fine 0.12 del 0.4 amp 0.5 cutoff 0.67 release 2 mix 0.7]
  (let [freq (midicps (+ note fine))
        osc  (pulse freq 0.25)
        src  (rlpf osc (lin-exp cutoff 0.0 1.0 20.0 20000.0) 0.15)
        src  (* src (env-gen (perc) :action FREE))
        del  (comb-n src 2 0.25 8)
        fad  (x-fade2 src del (- (* mix 2) 1) 1)
        src  (+ src fad)
        env  (env-gen (perc :release release) :action FREE)]
   (pan2 (* env amp src))))

(defsynth fx-echo-amp [bus 0 max-delay 1.0 delay-time 0.4 decay-time 2.0 amp 0.5]
  (let [source (in bus)
        echo (comb-n source max-delay delay-time decay-time)]
    (replace-out bus (pan2 (+ (* amp echo) source) 0))))

(comment
  (def echo (inst-fx! bass fx-echo-amp))
  (ctl echo :delay-time 0.04 :decay-time 0.2 :amp 0.35))

(def brvb (inst-fx! bouncy fx-freeverb))
(ctl brvb :room-size 0.2)

(comment
  (clear-fx bouncy)
  (clear-fx bass))

(definst strings [note 60 sustain 1.7 release 0.4 amp 0.15 cutoff 0.1 contour 0.3]
  (let [notes [note (- 0.22 note) (+ 0.14 note) (+ 0.40 note)]
        freqs (map midicps notes)
        src   (saw freqs)
        fil-env (env-gen (adsr 0.1 0.9 0.1 0.6))
        snd   (bpf src (+ (* fil-env (* contour 10000))
                          (lin-exp cutoff 0.0 1.0 20.0 20000.0)))
        env   (env-gen (env-lin 0.01 sustain release) 1 1 0 1 FREE)]
    (out 0 (pan2 (* amp env snd)))))

(def kick   (sample "resources/lights/kick.wav"))
(def snare  (sample "resources/lights/snare.wav"))
(def shaker (sample "resources/lights/shaker.wav"))
(def tom    (sample "resources/lights/tom_flat.wav"))
(def crash  (sample "resources/lights/crash.wav"))

(defn shift-coll [coll by]
  (map (partial + by) coll))

(defn repeat-patt [patt cnt]
  (let [shifters (range 0 (* cnt 8) 8)]
    (into #{} (apply concat (map (partial shift-coll patt) shifters)))))

(defn twice [patt]
  (repeat-patt patt 2))

(def patterns
  {kick   (twice #{0 2 4 6})
   snare  (twice #{1 3 5 7 31/4})
   shaker (twice #{1/2 3/2 5/2 7/2 9/2 11/2 13/2 15/2})
   tom    #{7/2}
   crash  #{0}})

(def *beats (atom patterns))

(def *strings (atom {:amp 0}))
(def play-strings (partial play-with-controls #'strings *strings))

(def *bouncy (atom {:amp 0}))
(def play-bouncy (partial play-with-controls #'bouncy *bouncy))

(defn play-all []
  (let [nome (metronome 120) beat (nome)]
    (sequencer nome beat *beats 1/8 16)
    (player lights-strings lights-bass-control nome beat play-strings 16 64)
    (player lights {} nome beat play-bouncy 16 64)
    (player lights-bass lights-bass-control nome beat #'bass 16 64)))

(swap! *beats clear-vals)

(comment
  (play-all)
  (reset! *beats patterns)
  (swap! *strings assoc :amp 0.0)
  (swap! *strings assoc :amp 0.3)
  (swap! *bouncy assoc :amp 0.0)
  (swap! *bouncy assoc :amp 0.45)
  (stop))

