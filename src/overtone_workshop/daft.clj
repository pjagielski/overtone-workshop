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

(def around1 (sample "resources/daft/around1.wav"))
(def around2 (sample "resources/daft/around2.wav"))

(defsynth sampler [in 0 rate 1 amp 1 pos 0 out-bus 0]
  (let [output (play-buf 1 in rate :start-pos pos :action FREE)]
    (out out-bus (* amp (pan2 output)))))

(def b (audio-bus))

(def s-around1 (partial sampler around1 1 2.5 0))
(def s-around2-0 (partial sampler around2 1 2.5))
(def s-around2 (partial sampler around2 1 2.5 10000))

(defsynth b-lpf [in-bus 10 cutoff 1.0]
  (out 0 (pan2
           (lpf (in in-bus) (lin-exp cutoff 0.0 1.0 20.0 20000.0)))))

(def drum-bus (audio-bus))

(defn b-lpf-fn [sample {:keys [cutoff]}]
  (let [b-sampler (partial sampler sample 1 1 0 drum-bus)]
    (b-lpf [:after (b-sampler)] drum-bus cutoff)))

(comment
  (def b-around1 (partial sampler around1 1 2.5 0 b))
  (b-lpf [:after (b-around1)] b 1.0)
  (b-lpf [:after (b-around1)] b 0.75))

(def lucky-robot1 (sample "resources/daft/lucky_robot1.wav"))
(def lucky-robot2 (sample "resources/daft/lucky_robot2.wav"))

(def s-lucky-robot1 (partial sampler lucky-robot1 1.08 1.7))
(def s-lucky-robot2 (partial sampler lucky-robot2 1.08 1.7))

(def h-rate 1.02)
(def h-amp 0.7)
(def s-work-it (partial sampler work-it h-rate h-amp))
(def s-make-it (partial sampler make-it h-rate h-amp))
(def s-do-it (partial sampler do-it h-rate h-amp))
(def s-makes-us (partial sampler makes-us h-rate h-amp))
(def s-harder (partial sampler harder h-rate h-amp))
(def s-better (partial sampler better h-rate h-amp))
(def s-faster (partial sampler faster h-rate h-amp))
(def s-stronger (partial sampler stronger h-rate h-amp 1000))

(definst bass [note 60 amp 0.5 osc-mix 0.25 cutoff 0.3 sustain 0.2 release 0.25 fil-dec 0.25 fil-amt 750 rq 0.4]
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

(defsynth funk [note 60 divisor 0.5 depth 1.0 sustain 0.2 contour 0.15 cutoff 0.5 amp 0.5]
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
                               (lin-exp cutoff 0.0 1.0 20.0 20000.0)) 0.05)]
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

(def *vocals (atom {}))
(def *arp (atom {:amp 0.0 :filt 3 :decay 0.4 :coef 0.1}))
(def *bass (atom {:amp 0}))
(def *funk (atom {:divisor 2 :depth 4.0 :cutoff 0.45 :amp 0.0}))

(def play-bass (partial play-with-controls #'bass *bass))
(def play-arp (partial play-with-controls #'ks *arp))
(def play-funk (partial play-with-controls #'funk *funk))

(def *beats-controls (atom {13 {:param :cutoff :min 0 :max 1}}))
(def *beats-flt (atom {:cutoff 1.0}))
(defn lpf-kick  [] (b-lpf-fn kick (deref *beats-flt)))
(defn lpf-snare [] (b-lpf-fn snare (deref *beats-flt)))
(defn lpf-open  [] (b-lpf-fn open-hat (deref *beats-flt)))
(def beats {lpf-kick #{0 1 2 3}})
(def *beats (atom beats))

(defn play-all []
  (let [nome (metronome 128) beat (nome)]
    (sequencer nome beat *vocals 1/2 32)
    (player dafunk dafunk-ctrls nome beat play-funk 16 64)
    (player around-arp-low {} nome beat play-arp 16 64)
    (player around-arp {} nome beat play-arp 16 64)
    (player around-bass around-bass-ctrl nome beat play-bass 16 64)
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

(defn next-loop [nome beats]
  (let [beat (nome)]
    (->> (mod beat beats) (- beat) (+ beats))))

(comment
  (swap! *beats-flt assoc :cutoff 0.3)
  (swap! *beats assoc lpf-kick #{0 1 2 3})
  (swap! *beats assoc lpf-snare #{1 3})
  (swap! *beats assoc clap #{1 3})
  (swap! *beats assoc close-hat #{0 1/2 3/2 2 5/2 7/2})
  (play-all)
  (player around-arp-low {} nome (next-loop nome 16) play-arp 16 64)
  (player around-arp {} nome (next-loop nome 16) play-arp 16 64)
  (untztrument *beats-flt *beats-controls)
  (untztrument *arp *controls)
  (remove-event-handler ::untztrument-control)
  (swap! *beats assoc lpf-kick #{1/2 3/2 5/2 7/2})
  (swap! *beats assoc lpf-open #{1/2 3/2 5/2 7/2})
  (dosync
    (swap! *beats assoc kick #{} snare #{})
    (swap! *funk assoc :amp 0.0)
    (swap! *bass assoc :amp 0.0)
    (swap! *arp assoc :amp 0.9)
    (swap! *vocals clear-vals))
  (dosync
    (swap! *beats assoc lpf-kick #{0 1 2 3})
    (swap! *beats assoc lpf-open #{1/2 3/2 5/2 7/2}))
  (reset! *beats beats)
  (swap! *beats clear-vals)
  (reset! *vocals vocals)
  (swap! *vocals clear-vals)
  (swap! *bass assoc :amp 0.3)
  (swap! *funk assoc :amp 0)
  (swap! *arp assoc :amp 0)
  (swap! *arp assoc :amp 0.9)
  (swap! *arp assoc :amp 0.5 :filt 24 :decay 0.9 :coef 0.1)
  (swap! *arp assoc :amp 0.5 :filt 6 :decay 0.4 :coef 0.1)
  (comment
    (swap! *vocals assoc s-lucky-robot1 #{})
    (swap! *vocals assoc s-around1 #{3/2} s-around2-0 #{9/2})
    (swap! *vocals assoc s-around1 #{} s-around2-0 #{})
    (swap! *vocals assoc s-work-it #{1} s-make-it #{3} s-do-it #{5} s-makes-us #{7}
                         s-harder #{17} s-better #{19} s-faster #{21} s-stronger #{23})
    (swap! *vocals assoc make-it #{1 5} harder #{3} better #{7})
    (swap! *vocals assoc s-work-it #{1} s-make-it #{3} s-do-it #{5} s-makes-us #{7})))
(stop)
