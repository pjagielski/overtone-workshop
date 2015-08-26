(ns overtone-workshop.timing
  (:use [overtone.live]
        [overtone.samples.piano :only [index-buffer]])
  (:require [overtone-workshop.player :refer :all]
            [overtone-workshop.patterns :refer :all]
            [overtone-workshop.sampler :refer :all]))

;; samples
(def kick (sample "resources/kick2.wav"))
(def snare (sample "resources/sd.wav"))
(def sd (sample "resources/m_sd.wav"))
(def sdl (sample "resources/sdl.wav"))
(def hat (sample "resources/tambo.wav"))
(def hihat (sample "resources/hihat.wav"))
(def ohh (sample "resources/openhh.wav"))
(def clap (sample "resources/clap.wav"))
(def congall (sample "resources/congall.wav"))
(def daften (sample "resources/daft/dafunk.aif"))

(def h_kick (sample "resources/house/kick.wav"))
(def f_kick (sample "resources/house/fat_bd.wav"))
(def h_clap (sample "resources/house/clap.wav"))
(def h_snare (sample "resources/house/snare.wav"))
(def h_hat (sample "resources/daft/close_hat.aif"))
(def h_ohat (sample "resources/daft/open_hat.aif"))

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
(defn loop-fn []
  (let [t (now) next (+ 1000 t)]
    (at t (kick))
    (apply-by next loop-fn [])))

(comment
  (loop-fn)
  (stop))

;; metronome
(def nome (metronome 128))
(nome)
(nome 3)

(defn loop-player [nome beat]
  (let [next-beat (inc beat)]
    (at (nome beat) (kick))
    (apply-by (nome next-beat) loop-player [nome next-beat])))

(comment
  (loop-player nome (nome))
  (nome :bpm 160)
  (nome :bpm 240)
  (nome :bpm 540)
  (stop))

(def _ 0)
(def beats {kick [1 _ _ _ 1 _ _ _]
            sd   [_ _ 1 _ _ _ 1 _]
            clap [_ _ 1 _ _ _ 1 _]
            hat  [_ _ _ _ _ _ _ _]})

(def *beats (atom beats))

(defn live-sequencer [nome beat live-patterns scale idx]
  (doseq [[sound pattern] @live-patterns]
     (when (= 1 (nth pattern (mod idx (count pattern))))
       (at (nome beat) (sound))))
  (let [next-beat (+ scale beat)]
    (apply-by (nome next-beat) live-sequencer [nome next-beat live-patterns scale (inc idx)])))

(comment
  (let [nome (metronome 128)]
    (live-sequencer nome (nome) *beats 1/2 0))
  (swap! *beats assoc kick [1 _ _ 1 _ 1 _ 1])
  (swap! *beats assoc hat  [_ 1 _ 1 _ 1 _ 1])
  (reset! *beats beats)
  (stop))

(def voc (partial slicer :in daften :start 0 :end 38000 :amp 1.0 :fade 0.0))

(comment
  (daften)
  (voc))

(def house-beats
  {kick   [1 _ _ _ 1 _ _ _ 1 _ _ _ 1 _ _ _ ]
   sd     [_ _ _ _ 1 _ _ _ _ _ _ _ 1 _ _ 1 ]
   clap   [_ _ _ _ 1 _ _ _ _ _ _ _ 1 _ _ _ ]
   h_hat  [_ _ 1 _ _ _ 1 1 _ _ 1 _ _ _ 1 1 ]
   hihat  [_ _ _ 1 _ _ _ _ _ 1 _ 1 _ _ _ 1 ]})

(def *house-beats (atom house-beats))

(comment
  (let [nome (metronome 120) beat (nome)]
    (live-sequencer nome beat *house-beats 1/4 0))
  (swap! *house-beats assoc voc [_ _ 1 _ _ _ _ _ _ _ _ _ _ _ _ 1])
  (stop))

(def hip-hop
  {kick    [1 _ _ _ _ _ _ 1 _ _ 1 _ _ _ _ 1 ]
   sdl     [_ _ _ _ 1 _ _ _ _ _ _ _ 1 _ _ _ ]
   clap    [_ _ _ _ 1 _ _ _ _ _ _ _ 1 _ _ _ ]
   hihat   [1 _ 1 _ 1 _ 1 _ 1 _ 1 _ 1 _ _ 1 ]
   ohh     [_ _ _ _ _ _ _ _ _ _ _ _ _ _ 1 _ ]})

(def *hip-hop (atom hip-hop))

(comment
  (swap! *hip-hop assoc congall [1 1 _ 1 _ _ _ _ _ _ 1 _ _ _ _ _])
  (let [nome (metronome 90) beat (nome)]
    (live-sequencer nome beat *hip-hop 1/4 0))
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
    (at (nome (+ 4 beat)) (ctl d :wobble 6 :note 45))
    (apply-by (nome (+ 6 beat)) skrillex [d nome])))

(comment
  (def d (dubstep 120))
  (ctl d :wobble 4)
  (ctl d :wobble 8)
  (ctl d :wobble 2 :note 31)
  (let [nome (metronome 120)]
    (skrillex (dubstep (nome :bpm)) nome))
  (stop))

(definst sampled-piano
  [note 60 level 1 rate 1 loop? 0 pos 0 attack 0 decay 1 sustain 1 release 0.1 curve -4 gate 1 cutoff 0.5]
  (let [buf (index:kr (:id index-buffer) note)
        env (env-gen (adsr attack decay sustain release level curve) :gate gate :action FREE)
        snd (scaled-play-buf 2 buf :start-pos pos :rate rate :level level :loop loop? :action FREE)
        flt (hpf snd (lin-exp cutoff 0.0 1.0 20.0 20000.0))]
    (* env flt)))

(definst bass [note 60 amp 0.7 osc-mix 0.6 cutoff 0.4 sustain 0.1 release 0.25 fil-dec 0.10 fil-amt 750]
  (let [note (- note 12)
        freq (midicps note)
        sub-freq (midicps (- note 12))
        osc1 (saw freq)
        osc2 (pulse sub-freq 0.5)
        osc (+ (* osc-mix osc2) (* (- 1 osc-mix) osc1))
        snd [osc osc]
        fil-env (env-gen (adsr 0.0 fil-dec 0.1 fil-dec))
        snd (lpf snd (+ (* fil-env fil-amt) (lin-exp cutoff 0.0 1.0 20.0 20000.0)))
        env (env-gen (env-lin 0.01 sustain release) 1 1 0 1 FREE)]
    (out 0 (* amp env snd))))

(def my-piano
  (partial sampled-piano :level 1.0 :pos 500 :decay 0.01 :cutoff 0.55))

(comment
  (bass)
  (sampled-piano :note (note :C4))
  (sampled-piano :note (note :A4))
  (sampled-piano :cutoff 0.1)
  (sampled-piano :cutoff 0.55)
  (reset! *house-beats {})
  (reset! *house-beats house-beats)
  (let [nome (metronome 122) beat (nome)]
    (live-sequencer nome beat *house-beats 1/4 0)
    (player follow {} nome beat #'my-piano 16 64)
    (player follow-bass {} nome beat #'bass 16 64))
  (stop))

