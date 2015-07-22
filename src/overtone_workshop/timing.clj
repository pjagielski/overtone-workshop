(ns overtone-workshop.timing
  (:use [overtone.live]
        [overtone.samples.piano :only [index-buffer]])
  (:require [overtone-workshop.player :refer :all]
            [overtone-workshop.patterns :refer :all]))

;; samples
(def kick (sample "resources/daft/kick.aif"))
(def snare (freesound-sample 82583))
(def hat (sample "resources/tambo.wav"))

(comment
  (kick)
  (snare)
  (hat))

;; timing
(now)

(comment
  (at (+ 1000 (now)) (kick))
  (at (+ 1000 (now)) (println "echo"))

  (let [t (now)]
    (at t (kick))
    (at (+ 1000 t) (kick))))

;; temporal recursion
(defn beats []
  (let [t (now) next (+ 1000 t)]
    (at t (kick))
    (apply-by next beats [])))

(comment
  (beats)
  (stop))

;; metronome
(def nome (metronome 128))
(nome)
(nome 3)

(defn beat-player [nome beat]
  (let [next-beat (inc beat)]
    (at (nome beat) (kick))
    (apply-by (nome next-beat) beat-player [nome next-beat])))

(comment
  (beat-player nome (nome))
  (nome :bpm 160)
  (nome :bpm 240)
  (nome :bpm 540)
  (stop))

(def _ 0)
(def pats {kick  [1 _ _ _ 1 _ _ _]
           snare [_ _ 1 _ _ _ 1 _]
           hat   [_ _ _ _ _ _ _ _]})

(def live-pats (atom pats))

(defn live-sequencer [nome beat live-patterns scale idx]
  (doseq [[sound pattern] @live-patterns]
     (when (= 1 (nth pattern (mod idx (count pattern))))
       (at (nome beat) (sound))))
  (let [next-beat (+ scale beat)]
    (apply-by (nome next-beat) live-sequencer [nome next-beat live-patterns scale (inc idx)])))

(comment
  (let [nome (metronome 128)]
    (live-sequencer nome (nome) live-pats 1/2 0))
  (swap! live-pats assoc kick [1 _ _ 1 _ 1 _ 1])
  (swap! live-pats assoc hat  [_ 1 _ 1 _ 1 _ 1])
  (reset! live-pats pats)
  (stop))

(defsynth dubstep [bpm 100 wobble 1 note 29 v 1 out-bus 0]
 (let [trig (impulse:kr (/ bpm 120))
       freq (midicps note)
       swr (demand trig 0 (dseq [wobble] INF))
       sweep (lin-exp (lf-tri swr) -1 1 40 3000)
       wob (apply + (saw (* freq [0.99 1.01])))
       wob (lpf wob sweep)
       wob (* 0.5 (normalizer wob))
       wob (+ wob (bpf wob 1500 2))
       wob (+ wob (* 0.2 (g-verb wob 11 0.7 0.7)))]
   (out out-bus wob)))

(defn skrillex [d nome]
  (let [beat (nome)]
    (at (nome) (ctl d :wobble 1.5 :note 29))
    (at (nome (+ 2 beat)) (ctl d :wobble 4 :note 40))
    (at (nome (+ 6 beat)) (ctl d :wobble 6 :note 45))
    (apply-by (nome (+ 8 beat)) skrillex [d nome])))

(comment
  (def d (dubstep 120))
  (ctl d :wobble 4)
  (ctl d :wobble 8)
  (ctl d :wobble 2 :note 31)
  (let [nome (metronome 120)]
    (skrillex (dubstep (nome :bpm)) nome))
  (stop))

(def h_kick (sample "resources/house/kick.wav"))
(def f_kick (sample "resources/house/fat_bd.wav"))
(def h_clap (sample "resources/house/clap.wav"))
(def h_snare (sample "resources/house/snare.wav"))
(def h_hat (sample "resources/daft/close_hat.aif"))

(def h_pats {h_kick  #{0 1 2 3 4 5 6 7}
             h_snare #{1 3 5 7}
             h_clap  #{1 3 5 7}
             h_hat   (into #{} (concat (range 0 8 1/2)
                                       (shift-patt 0 [3/4 7/4 5/4])
                                       (shift-patt 4 [3/4 7/4 5/4])))})

(def *pats (atom h_pats))

(definst sampled-piano
  [note 60 level 1 rate 1 loop? 0 pos 0 attack 0 decay 1 sustain 1 release 0.1 curve -4 gate 1]
  (let [buf (index:kr (:id index-buffer) note)
        env (env-gen (adsr attack decay sustain release level curve) :gate gate :action FREE)]
    (* env (scaled-play-buf 2 buf :start-pos pos :rate rate :level level :loop loop? :action FREE))))

(definst bass [note 60 amp 0.5 osc-mix 0.2 cutoff 0.35 sustain 0.25 release 0.25 fil-dec 0.25 fil-amt 1000]
  (let [note (- note 12)
        freq (midicps note)
        sub-freq (midicps (- note 12))
        osc1 (saw:ar freq)
        osc2 (pulse sub-freq 0.5)
        osc (+ (* osc-mix osc2) (* (- 1 osc-mix) osc1))
        snd [osc osc]
        fil-env (env-gen (adsr 0.0 fil-dec 0.1 fil-dec))
        snd (lpf snd (+ (* fil-env fil-amt) (lin-exp cutoff 0.0 1.0 20.0 20000.0)))
        env (env-gen (env-lin 0.01 sustain release) 1 1 0 1 FREE)]
    (out 0 (* amp env snd))))

(def my-piano
  (partial sampled-piano :level 0.4 :pos 1500 :decay 0.1))

(comment
  (swap! *pats assoc h_snare #{1 3 5 25/4 7})
  (reset! *pats h_pats)
  (reset! *pats {})
  (swap! *pats assoc h_snare #{1 3 5 7})
  (swap! *pats assoc h_clap #{1 3 5 7})
  (swap! *pats assoc h_kick #{0 2 5/4 4 6 7 15/2})
  (swap! *pats assoc h_hat #{0 1/2 3/4 1 5/4 7/4 4 5 6 7})
  (let [nome (metronome 122) beat (nome)]
    (sequencer nome beat *pats 1/4 8)
    (player follow {} nome beat #'my-piano 16 64)
    (player follow-bass {} nome beat #'bass 16 64))
  (stop))

