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
(def daften    (sample "resources/daft/dafunk.aif"))
(def techno    (sample "resources/daft/technologic.wav"))
(def ptoyl     (sample "resources/daft/ptoyl.wav"))

(def work-it  (sample "resources/work_it.wav"))
(def make-it  (sample "resources/make_it.wav"))
(def do-it    (sample "resources/do_it.wav"))
(def makes-us (sample "resources/makes_us.wav"))
(def harder   (sample "resources/harder.wav"))
(def better   (sample "resources/better.wav"))
(def faster   (sample "resources/faster.wav"))
(def stronger (sample "resources/stronger.wav"))

(def around1 (sample "resources/daft/around1.wav"))
(def around2 (sample "resources/daft/around2.wav"))

(def music (sample "resources/daft/msbwy.wav"))
(def phoenix (sample "resources/daft/phoenix.wav"))

(def daften-clip (partial slicer :in daften :start 1000 :end 38000 :amp 1.0))
(def phoenix-clip (partial slicer :in phoenix :start 50000 :end 125000 :amp 1.0 :rate 1.08))
(def techo-clip (partial sampler techno :amp 0.75 :rate 1.01))
(def ptoyl-clip (partial sampler ptoyl :amp 0.75 :rate 1.02))

(def b (audio-bus))

(def s-around1 (partial sampler around1 1 1 0))
(def s-around2-0 (partial sampler around2 1 1))
(def s-around2 (partial sampler around2 1 1.5 10000))

(defsynth b-lpf [in-bus 10 cutoff 1.0 room 0.5]
  (let [snd (lpf (in in-bus) (lin-exp cutoff 0.0 1.0 20.0 20000.0))
        fx  (free-verb snd :room room)]
    (out 0 (pan2 fx))))

(def drum-bus (audio-bus))

(defn b-lpf-fn [sample {:keys [cutoff room]}]
  (let [b-sampler (partial sampler sample 1 1 0 drum-bus)]
    (b-lpf [:after (b-sampler)] drum-bus cutoff room)))

(comment
  (def b-around1 (partial sampler around1 1 2.5 0 b))
  (b-lpf [:after (b-around1)] b 1.0)
  (b-lpf [:after (b-around1)] b 0.75))

(def lucky-robot1 (sample "resources/daft/lucky_robot1.wav"))
(def lucky-robot2 (sample "resources/daft/lucky_robot2.wav"))

(def s-lucky-robot1 (partial sampler lucky-robot1 1.08 1.7))
(def s-lucky-robot2 (partial sampler lucky-robot2 1.08 1.7))

(def h-rate 1.02)
(def h-amp 0.4)
(def s-work-it (partial sampler work-it h-rate h-amp))
(def s-make-it (partial sampler make-it h-rate h-amp))
(def s-do-it (partial sampler do-it h-rate h-amp))
(def s-makes-us (partial sampler makes-us h-rate h-amp))
(def s-harder (partial sampler harder h-rate h-amp))
(def s-better (partial sampler better h-rate h-amp))
(def s-faster (partial sampler faster h-rate h-amp))
(def s-stronger (partial sampler stronger h-rate h-amp 1000))

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

(def *beats-controls (atom {13 {:param :cutoff :min 0.6 :max 1}
                            11 {:param :room :min 0.1 :max 1}}))
(def *beats-flt (atom {:cutoff 1.0 :room 0.5}))
(defn lpf-kick  [] (b-lpf-fn kick (deref *beats-flt)))
(defn lpf-snare [] (b-lpf-fn snare (deref *beats-flt)))
(defn lpf-open  [] (b-lpf-fn open-hat (deref *beats-flt)))

(def beats {kick (into #{} (range 0 8))})
(def *beats (atom beats))

(defn slicer-player [nome beat]
  (let [next-beat (+ (rand-nth [1 2]) beat)
        start (ranged-rand 75000 150000)
        dur (ranged-rand 5200 8550)]
    (at (nome beat) (slicer music start (+ dur start)))
    (apply-by (nome next-beat) #'slicer-player [nome next-beat])))

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

(comment
  (dosync
    (def pad-rev (inst-fx! overpad fx-reverb))
    (ctl pad-rev :room 0.5 :mix 0.2))
  (dosync
    (def pad-echo (inst-fx! overpad fx-echo))
    (ctl pad-echo :delay-time 0.6 :decay-time 0.6))
  (clear-fx overpad))

(def play-bass (partial play-with-controls #'my-bass *bass))

(defn play-all []
  (let [nome (metronome 128) beat (nome)]
    (sequencer nome beat *vocals 1/4 16)
    (player giorgio {} nome beat #'play-pad 16 64)
    (player around-arp-low {} nome beat #'play-arp-low 16 64)
    (player around-arp {} nome beat #'play-arp 16 64)
    (player within {} nome beat #'play-bass 16 64)
    (sequencer nome beat *beats 1/4 8)
    beat))

(def *controls (atom {13 {:param :amp :min 0 :max 1.0}
                      11 {:param :filt :min 1 :max 24}
                       1 {:param :coef :min 0.1 :max 1.0}}))

(def *cut-controls (atom {13 {:param :amp :min 0 :max 1.0}
                          11 {:param :cutoff :min 0 :max 1.0}
                           1 {:param :rq :min 0 :max 1.0}}))

(comment
  (play-all)
  (stop)
  (swap! *beats assoc kick_l (into #{} (range 0 8 1/4)))
  (swap! *beats assoc kick (into #{} (range 0 8 1/4)))
  (swap! *beats assoc snare (into #{} (range 0 8 1/4)))
  (swap! *beats assoc clap (into #{} (range 1/4 8 1/4)))
  (swap! *beats assoc tom (into #{} (range 1/4 8 5/4)))
  (dosync
    (swap! *beats assoc snare #{1 3 5 7})
    (swap! *beats assoc kick_l (into #{} (range 0 8 1)))
    (swap! *beats assoc kick (into #{} (range 0 8 1)))
    (swap! *beats assoc clap #{1 3 5 7})
    (swap! *beats assoc close-hat (into #{} (range 0 8 1)))
    (swap! *beats assoc highhat (into #{} (concat (range 1/2 8 1) [13/4])))
    (swap! *beats assoc open-hat #{1/2 3/2 5/2 7/2 9/2 11/2 13/2 15/2}))
  (swap! *beats assoc tom #{})
  (untztrument play-arp *bass *cut-controls)
  (untztrument play-arp *beats-flt *beats-controls)
  (untztrument play-arp *arp-low *controls)
  (untztrument play-arp *arp *controls)
  (remove-event-handler ::untztrument-control)
  (swap! *beats assoc tom
         (into #{} (concat [1/2 7/4 5/2]
                           (map (partial + 4) [1/2 7/4 5/2]))))
  (swap! *beats assoc highhat (into #{} (concat (range 1/4 8 1) [13/4])))
  (dosync
    (reset! *beats beats)
    (swap! *vocals assoc techo-clip #{0 8})
    (swap! *pad assoc :amp 0.1))
  (dosync
    (reset! *beats beats)
    (swap! *vocals assoc techo-clip #{} ptoyl-clip #{}
                         s-around1 #{3/2} s-around2-0 #{9/2})
    (swap! *pad assoc :amp 0.0)
    (swap! *arp-low assoc :amp 0.5 :filt 12 :decay 0.9 :coef 0.1)
    (swap! *bass assoc :amp 0.2 :cutoff 0.65 :rq 0.4))
  (reset! *beats beats)
  (swap! *beats clear-vals)
  (swap! *vocals clear-vals)
  (swap! *pad assoc :amp 0.2 :release 0.2)
  (swap! *bass assoc :amp 0.0)
  (swap! *bass assoc :amp 0.2 :cutoff 0.85 :rq 0.5)
  (swap! *beats-flt assoc :cutoff 0.99)
  (dosync
    (swap! *bass assoc :amp 0.0)
    (swap! *arp assoc :amp 0)
    (swap! *arp-low assoc :amp 0))
  (swap! *arp-low assoc :amp 0.9)
  (swap! *arp-low assoc :amp 0.5 :filt 24 :decay 0.9 :coef 0.1)
  (swap! *arp assoc :amp 0.9 :filt 24 :decay 0.9 :coef 0.1)
  (swap! *arp assoc :amp 0.5 :filt 6 :decay 0.4 :coef 0.1)
  (play-all)
  (comment
    (swap! *vocals assoc ptoyl-clip #{4})
    (swap! *vocals assoc techo-clip #{0 8})
    (swap! *vocals assoc daften-clip #{3/4})
    (swap! *vocals assoc s-around1 #{3/2} s-around2-0 #{})
    (swap! *vocals assoc s-around1 #{3/2} s-around2-0 #{9/2})))
(stop)
