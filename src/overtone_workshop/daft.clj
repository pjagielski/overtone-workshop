(ns overtone-workshop.daft
  (:use [overtone.live])
  (:require [overtone-workshop.player :refer :all]
            [overtone-workshop.patterns :refer :all]))

(def kick      (sample "resources/daft/kick.aif"))
(def snare     (sample "resources/daft/snare.aif"))
(def clap      (sample "resources/daft/clap.aif"))
(def open-hat  (sample "resources/daft/open_hat.aif"))
(def close-hat (sample "resources/daft/close_hat.aif"))

(def work-it  (sample "resources/work_it.wav"))
(def make-it  (sample "resources/make_it.wav"))
(def do-it    (sample "resources/do_it.wav"))
(def makes-us (sample "resources/makes_us.wav"))
(def harder   (sample "resources/harder.wav"))
(def better   (sample "resources/better.wav"))
(def faster   (sample "resources/faster.wav"))
(def stronger (sample "resources/stronger.wav"))

(def dp {60 harder  61 better  62 faster 63 stronger
         64 work-it 65 make-it 66 do-it  67 makes-us })

(on-event [:midi :note-on]
    (fn [{n :note}] (apply (get dp n) []))
    ::midi-player)
(remove-event-handler ::midi-player)

(def around1 (sample "resources/daft/around1.wav"))
(def around2 (sample "resources/daft/around2.wav"))

(defsynth sampler [in 0 rate 1 amp 1 pos 0]
  (let [output (play-buf 1 in rate :start-pos pos :action FREE)]
    (out 0 (* amp (pan2 output)))))

(def s-around1 (partial sampler around1 1 2.5))
(def s-around2-0 (partial sampler around2 1 2.5))
(def s-around2 (partial sampler around2 1 2.5 10000))

(def lucky-robot1 (sample "resources/daft/lucky_robot1.wav"))
(def lucky-robot2 (sample "resources/daft/lucky_robot2.wav"))
(def s-lucky-robot1 (partial sampler lucky-robot1 1.08 1.7))
(def s-lucky-robot2 (partial sampler lucky-robot2 1.08 1.7))

(definst bass [note 60 amp 0.7 cutoff 0.4 sustain 0.2 release 0.25 fil-dec 0.1 fil-amt 1500]
  (let [freq  (midicps note)
        osc1  (saw freq)
        osc2  (saw (midicps (- note 0.12)))
        osc   (+ osc1 osc2)
        snd   [osc1 osc1]
        f-env (env-gen (adsr 0.0 fil-dec 0.3 fil-dec))
        snd   (lpf snd (+ (* f-env fil-amt) (lin-exp cutoff 0.0 1.0 20.0 20000.0)))
        env   (env-gen (env-lin 0.001 sustain release) 1 1 0 1 FREE)]
    (out 0 (* amp env snd))))

(defsynth basshigh [note 60 sustain 0.2 cutoff 4000 drive 0.75 amp 0.1 rq 0.8]
  (let [freq    (midicps note)
        freqs   (map (partial * freq) [0.25 1 1.25])
        osc     (mix (saw freqs))
        fil-env (env-gen (adsr 0.0 0.5 0.2 0.2) 1 :action FREE)
        fltr    (rlpf osc (+ 100 (* cutoff fil-env)) rq)
        k       (/ (* 2 drive) (- 1 drive))
        fltr    (/ (* fltr (+ 1 k)) (+ 1 (* k (abs fltr))))
        fltr    (b-low-shelf:ar fltr 300 1.0 -12.0)
        fltr    (b-peak-eq:ar fltr 1600 1.0 -6.0)
        env     (env-gen (env-lin 0.01 sustain 0.1) 1 1 0 1 FREE)]
    (out 0 (pan2 (* fltr env amp)))))

(defsynth funk [note 60 divisor 0.5 depth 1.0 sustain 0.2 contour 0.15 cutoff 0.4 amp 0.5]
  (let [carrier   (midicps note)
        modulator (/ carrier divisor)
        freq      (midicps (+ note 0.12))
        mod-env   (env-gen (lin 0.1 sustain 0.2))
        amp-env   (env-gen (env-lin 0.01 sustain 0.2) :action FREE)
        fil-env   (env-gen (adsr 0.1 0.45 0.1 0.2))
        osc1      (* 0.5 (saw (/ freq 2)))
        osc2      (saw modulator)
        mod-osc2  (sin-osc (+ carrier
                              (* mod-env (* carrier depth) osc2)))
        snd       (+ mod-osc2 osc1)
        snd       (rlpf snd (+ (* fil-env (* contour 10000))
                               (lin-exp cutoff 0.0 1.0 20.0 20000.0)) 0.15)]
    (out 0 (pan2 (* amp amp-env snd)))))

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

(def vocals
  {work-it    #{} make-it  #{}
   do-it      #{} makes-us #{}
   s-around1   #{3/2 19/2}
   s-around2-0 #{9/2 25/2}
   s-around2   #{}
   s-lucky-robot1 #{}
   s-lucky-robot2 #{}})

(def *vocals (atom vocals))

(def beats
  {kick      #{0 1 2 3}
   snare     #{1 3}
   clap      #{1 3}
   open-hat  #{}
   close-hat #{0 1/2 3/2 2 5/2 7/2}})

(def *beats (atom beats))

(def *arp (atom {:amp 0 :filt 3}))
(def *bass (atom {:amp 0.5}))
(def *funk (atom {:divisor 4 :depth 1.5 :amp 0.0}))

(defn play-bass []
  (partial play-with-controls #'bass *bass))

(defn play-high-bass []
  (partial play-with-controls #'basshigh *bass))

(defn play-arp []
  (partial play-with-controls #'ks *arp))

(defn play-funk []
  (partial play-with-controls #'funk *funk))

(defn play-all []
  (let [nome (metronome 126) beat (nome)]
    (sequencer nome beat *vocals 1/2 8)
    (player dafunk dafunk-ctrls nome beat (play-funk) 16 64)
    (player around-arp {} nome beat (play-arp) 16 64)
    (player around-bass around-bass-ctrl nome beat (play-bass) 16 64)
    (player around-bass around-bass-ctrl nome beat (play-high-bass) 16 64)
    (sequencer nome beat *beats 1/2 4)))

(def *controls (atom {13 {:param :amp :min 0 :max 1.0}
                      11 {:param :filt :min 1 :max 24}
                       1 {:param :coef :min 0.1 :max 1.0}}))

(defn untztrument [synth-controls controls]
  (on-event [:midi :control-change]
            (fn [{value :velocity note :note}]
              (when-let [control (get @controls note)]
                (let [normalized-value (/ (- value 1) 127)
                      scaled-value (+ (:min control) (* normalized-value (- (:max control) (:min control))))]
                  (swap! synth-controls assoc (:param control) scaled-value))))
               ::untztrument-control))

(comment
  (swap! *vocals clear-vals)
  (swap! *funk assoc :amp 0.4)
  (play-all)
  (untztrument *arp *controls)
  (remove-event-handler ::untztrument-control)
  (swap! *beats assoc kick #{1/2 3/2 5/2 7/2})
  (swap! *beats assoc open-hat #{1/2 3/2 5/2 7/2})
  (dosync
    (swap! *bass assoc :amp 0.0)
    (reset! *vocals vocals))
  (dosync
    (swap! *beats assoc kick #{} snare #{})
    (swap! *funk assoc :amp 0.0)
    (swap! *vocals clear-vals))
  (dosync
    (swap! *bass assoc :amp 0.5)
    (swap! *funk assoc :amp 0.0))
  (dosync
    (swap! *bass assoc :amp 0.0)
    (swap! *funk assoc :amp 0.4))
  (reset! *beats beats)
  (swap! *beats clear-vals)
  (swap! *vocals clear-vals)
  (swap! *bass assoc :amp 0)
  (swap! *funk assoc :amp 0)
  (swap! *arp assoc :amp 0)
  (swap! *arp assoc :amp 0.5)
  (swap! *arp assoc :amp 0.5 :filt 24 :decay 0.9 :coef 0.1)
  (comment
    (swap! *vocals assoc s-lucky-robot1 #{})
    (swap! *vocals assoc s-around1 #{3/2 19/2} s-around2-0 #{9/2 25/2})
    (swap! *vocals assoc s-around1 #{} s-around2-0 #{})
    (swap! *vocals assoc make-it #{1 3 5 7})
    (swap! *vocals assoc make-it #{1 5} harder #{3} better #{7})
    (swap! *vocals assoc work-it #{1} make-it #{3} do-it #{5} makes-us #{7})))

(stop)

