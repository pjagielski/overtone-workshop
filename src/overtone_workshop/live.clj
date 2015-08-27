(ns overtone-workshop.live
  (:use [overtone.live])
  (:require [overtone.inst.synth :refer [overpad]]
            [overtone-workshop.player :refer :all]
            [overtone-workshop.patterns :refer :all]
            [overtone-workshop.untztrument :refer :all]
            [overtone-workshop.sampler :refer :all]))

(def kick_l    (sample "resources/kick.wav"))
(def kick      (sample "resources/kick2.wav"))
(def snare     (sample "resources/sdl.wav"))
(def clap      (sample "resources/daft/clap.aif"))
(def open-hat  (sample "resources/daft/open_hat.aif"))
(def close-hat (sample "resources/daft/close_hat.aif"))
(def tom       (sample "resources/congah.wav"))
(def highhat   (sample "resources/hihat.wav"))
(def shaker    (sample "resources/lights/shaker.wav"))
(def techno    (sample "resources/daft/technologic.wav"))
(def ptoyl     (sample "resources/daft/ptoyl.wav"))
(def around1 (sample "resources/daft/around1.wav"))
(def around2 (sample "resources/daft/around2.wav"))

(def techo-clip (partial sampler techno :amp 0.25 :rate 1.01))
(def ptoyl-clip (partial sampler ptoyl :amp 0.55 :rate 1.02))

(def s-around1 (partial sampler around1 :amp 1.2 :rate 1))
(def s-around2-0 (partial sampler around2 1 1))

(definst bass [note 60 amp 0.5 osc-mix 0.25 cutoff 0.3 sustain 0.2 release 0.25 fil-dec 0.25 fil-amt 500 rq 0.35]
  (let [freq (midicps note)
        sub-freq (midicps (- note 12))
        osc1 (saw:ar freq)
        osc2 (pulse sub-freq 0.75)
        osc (+ (* osc-mix osc2) (* (- 1 osc-mix) osc1))
        snd [osc osc]
        fil-env (env-gen (adsr 0.0 fil-dec 0.1 fil-dec))
        snd (rlpf snd (+ (* fil-env fil-amt) (lin-exp cutoff 0.0 1.0 20.0 20000.0)) rq)
        env (env-gen (env-lin 0.01 sustain release) 1 1 0 1 FREE)]
    (out 0 (* amp env snd))))

(definst ks [note 60 amp 0.8 dur 2 decay 5 coef 0.5 filt 6]
  (let [freq   (midicps note)
        noize  (* 0.99 (white-noise))
        dly    (/ 1 freq)
        plk    (pluck noize 0.2 (/ 1.0 freq) dly decay coef)
        dist   (distort plk)
        filt   (rlpf dist (* filt freq) 0.2)
        ;filt   (bpf plk (* filt freq))
        clp    (clip2 filt 0.2)
        reverb (free-verb clp 0.5 0.9 0.9)]
    (* amp (env-gen (perc 0.01 dur) :action FREE) reverb)))

(def *vocals (atom {}))
(def *arp-low (atom {:amp 0.0 :filt 3 :decay 0.4 :coef 0.1}))
(def *arp (atom {:amp 0.0 :filt 3 :decay 0.4 :coef 0.1}))
(def *bass (atom {:amp 0 :cutoff 0.0 :rq 0.5}))

(def play-arp (partial play-with-controls #'ks *arp))
(def play-arp-low (partial play-with-controls #'ks *arp-low))

(def beats {kick (into #{} (range 0 8))})
(def *beats (atom beats))

(definst my-bass [note 60 amp 0.35 cutoff 0.12 rq 0.5]
  (let [osc1 (pulse (midicps note) 0.45)
        osc2 (saw (midicps (+ note 7)))
        src  (+ osc1 osc2)
        fil-env (env-gen (adsr 0.1 0.45 0.1 0.2))
        snd  (rlpf src (* fil-env (lin-exp cutoff 0.0 1.0 20.0 20000.0)) rq)
        env  (env-gen (perc 0.001 0.3) :action FREE)]
    (* env snd amp)))

(definst my-pad [note 60 amp 0.2]
  (* (env-gen (perc 0.001 0.4) :action FREE)
     (+ (pulse (midicps (- note 12)))
        (sin-osc (midicps (- note 7))))
     amp))

(def *pad (atom {:release 0.3 :amp 0.0}))
(def play-pad (partial play-with-controls #'overpad *pad))

(def play-bass (partial play-with-controls #'my-bass *bass))

(defn play-all []
  (let [nome (metronome 128) beat (nome)]
    (sequencer nome beat *vocals 1/4 16)
    (player giorgio {} nome beat #'play-pad 16 64)
    (player around-arp-low {} nome beat #'play-arp-low 16 64)
    (player around-arp {} nome beat #'play-arp 16 64)
    (player within {} nome beat #'play-bass 16 64)
    (sequencer nome beat *beats 1/8 8)
    beat))

(def *controls (atom {13 {:param :amp :min 0 :max 1.0}
                      11 {:param :filt :min 1 :max 24}
                       1 {:param :coef :min 0.1 :max 1.0}}))

(def *cut-controls (atom {13 {:param :amp :min 0 :max 1.0}
                          11 {:param :cutoff :min 0 :max 1.0}
                           1 {:param :rq :min 0 :max 1.0}}))

(def *pad-controls (atom {13 {:param :amp :min 0 :max 1.0}
                          11 {:param :release :min 0 :max 1.0}}))

(def *dummy-synth (atom {}))

(defn hihats []
  (swap! *beats assoc close-hat (into #{} (range 0 8 1)))
  (swap! *beats assoc highhat (into #{} (concat (range 1/2 8 1) [13/4])))
  (swap! *beats assoc open-hat #{1/2 3/2 5/2 7/2 9/2 11/2 13/2 15/2})
  )

(defn snares []
  (swap! *beats assoc snare #{1 3 5 7})
  (swap! *beats assoc clap #{1 3 5 7}))

(defn full-beats []
  (dosync
    (swap! *beats assoc snare #{1 3 5 7})
    (swap! *beats assoc kick_l (into #{} (range 0 8 1)))
    (swap! *beats assoc kick (into #{} (range 0 8 1)))
    (swap! *beats assoc clap #{1 3 5 7})
    (swap! *beats assoc close-hat (into #{} (range 0 8 1)))
    (swap! *beats assoc highhat (into #{} (concat (range 1/2 8 1) [13/4])))
    (swap! *beats assoc open-hat #{1/2 3/2 5/2 7/2 9/2 11/2 13/2 15/2})))

(defn part2 []
  (dosync
    (reset! *beats beats)
    (untztrument *arp-low *controls)
    (swap! *arp assoc :amp 0.5 :filt 6 :decay 0.4 :coef 0.1)))

(def reset-beats #(reset! *beats beats))
(def control-arp #(untztrument *arp *controls))
(def control-arp-low #(untztrument *arp-low *controls))
(def control-bass #(untztrument *bass *cut-controls))
(def control-pad #(untztrument *pad *pad-controls))
(def no-control #(untztrument *dummy-synth *pad-controls))

(def actions {60 reset-beats 61 full-beats 62 hihats 63 snares
              64 control-arp 65 control-arp-low 66 control-bass 67 control-pad
              72 part2 73 no-control})
(on-event [:midi :note-on]
    (fn [{note :note}]
      (prn note)
      (when-let [f (get actions note)]
        (f)))
    ::live-controls)

(comment
  (dosync
    (def pad-rev (inst-fx! overpad fx-reverb))
    (ctl pad-rev :room 0.5 :mix 0.2))
  (dosync
    (def pad-echo (inst-fx! overpad fx-echo))
    (ctl pad-echo :delay-time 0.2 :decay-time 0.1))
  (clear-fx overpad))

(comment
  (play-all)
  (stop)
  (inst-fx! ks fx-distortion)
  (clear-fx ks)
  (swap! *vocals clear-vals)
  (swap! *beats clear-vals)
  (reset-beats)
  (full-beats)
  (snares)
  (hihats)
  (part2)
  (swap! *beats assoc kick_l (into #{} (range 0 8 1/4)))
  (swap! *beats assoc kick (into #{} (range 0 8 1/4)))
  (swap! *beats assoc snare (into #{} (->> (range 0 8 3/4) (remove #(= 0 (mod % 1))))))
  (swap! *beats assoc snare (into #{} (range 1/2 8 3/2)))
  (swap! *beats assoc clap (into #{} (range 1/4 8 3/2)))
  (swap! *beats assoc tom (into #{} (range 1/4 8 5/4)))
  (swap! *beats assoc close-hat (into #{} (->> (range 0 8 3/4) (remove #(= 0 (mod % 3/2))))))
  (swap! *beats assoc tom #{})
  (untztrument *dummy-synth *pad-controls)
  (remove-event-handler ::untztrument-control)
  (untztrument *pad *pad-controls)
  (untztrument *bass *cut-controls)
  (untztrument *arp-low *controls)
  (untztrument *arp *controls)
  (swap! *beats assoc tom
         (into #{} (concat [1/2 7/4 5/2]
                           (map (partial + 4) [1/2 7/4 5/2]))))
  (swap! *beats assoc highhat (into #{} (concat (range 1/4 8 1) [13/4])))
  (dosync
    (reset! *beats beats)
    (swap! *pad assoc :amp 0.1 :release 0.1))
  (reset! *beats beats)
  (swap! *pad assoc :amp 0.2 :release 0.2)
  (swap! *bass assoc :amp 0.0)
  (swap! *bass assoc :amp 0.2 :cutoff 0.85 :rq 0.5)
  (dosync
    (untztrument *bass *cut-controls)
    (swap! *bass assoc :amp 0.05)
    (swap! *arp assoc :amp 0)
    (swap! *arp-low assoc :amp 0))
  (swap! *arp-low assoc :amp 0.9)
  (swap! *arp-low assoc :amp 0.5 :filt 24 :decay 0.9 :coef 0.1)
  (swap! *arp assoc :amp 0.9 :filt 24 :decay 0.9 :coef 0.1)
  (swap! *arp assoc :amp 0.5 :filt 6 :decay 0.4 :coef 0.1)
  (play-all)
  (comment
    (swap! *vocals assoc #'ptoyl-clip #{4})
    (swap! *vocals assoc #'techo-clip #{0 8})
    (swap! *vocals assoc #'s-around1 #{3/2})
    (swap! *vocals assoc #'s-around1 #{3/2} #'s-around2-0 #{9/2})))
(stop)
