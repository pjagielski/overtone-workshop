(ns overtone-workshop.live
  (:use [overtone.live])
  (:require [overtone.inst.synth :refer [overpad]]
            [overtone-workshop.player :refer :all]
            [overtone-workshop.patterns :refer :all]
            [overtone-workshop.untztrument :refer :all]))

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

(def music (sample "resources/daft/msbwy.wav"))
(def phoenix (sample "resources/daft/phoenix.wav"))

(defsynth sampler [in 0 rate 1 amp 0.95 pos 0 out-bus 0]
  (let [output (play-buf 1 in rate :start-pos pos :action FREE)]
    (out out-bus (* amp (pan2 output)))))

(def b (audio-bus))

(def s-around1 (partial sampler around1 1 1 0))
(def s-around2-0 (partial sampler around2 1 1))
(def s-around2 (partial sampler around2 1 1.5 10000))

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

(defsynth funk [note 60 divisor 0.5 depth 1.0 sustain 0.2 contour 0.15 cutoff 0.5 amp 0.0]
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
(def *arp-low (atom {:amp 0.0 :filt 3 :decay 0.4 :coef 0.1}))
(def *arp (atom {:amp 0.0 :filt 3 :decay 0.4 :coef 0.1}))
(def *bass (atom {:amp 0 :cutoff 0.0}))
(def *funk (atom {:divisor 2 :depth 4.0 :cutoff 0.45 :amp 0.0}))

(def play-arp (partial play-with-controls #'ks *arp))
(def play-arp-low (partial play-with-controls #'ks *arp-low))
(def play-funk (partial play-with-controls #'funk *funk))

(def *beats-controls (atom {13 {:param :cutoff :min 0.6 :max 1}}))
(def *beats-flt (atom {:cutoff 1.0}))
(defn lpf-kick  [] (b-lpf-fn kick (deref *beats-flt)))
(defn lpf-snare [] (b-lpf-fn snare (deref *beats-flt)))
(defn lpf-open  [] (b-lpf-fn open-hat (deref *beats-flt)))
(def beats {lpf-kick #{0 1 2 3}})
(def *beats (atom beats))

(defsynth slicer [in 0 start 0 end 44100 fade 0.01 mul 1 amp 0.2]
  (let [phase (phasor:ar 0 (buf-rate-scale:kr in) start end)
        sig (buf-rd:ar 2 in phase 0)
        dur (/ (- end start)
               (- (buf-sample-rate:kr in) (* fade 2)))
        env (env-gen (env-lin fade dur fade mul) :action FREE)]
    (out 0 (pan2 (* sig env amp)))))

(defn slicer-player [nome beat]
  (let [next-beat (+ (rand-nth [1 2]) beat)
        start (ranged-rand 75000 150000)
        dur (ranged-rand 5200 8550)]
    (at (nome beat) (slicer music start (+ dur start)))
    (apply-by (nome next-beat) #'slicer-player [nome next-beat])))

(definst my-bass [note 60 amp 0.35 cutoff 0.12]
  (let [osc1 (pulse (midicps note) 0.45)
        osc2 (saw (midicps (+ note 7)))
        src  (+ osc1 osc2)
        fil-env (env-gen (adsr 0.1 0.45 0.1 0.2))
        snd  (rlpf src (* fil-env (lin-exp cutoff 0.0 1.0 20.0 20000.0)) 0.5)
        env  (env-gen (perc 0.001 0.4) :action FREE)]
    (* env snd amp)))

(definst my-pad [note 60 amp 0.2]
  (* (env-gen (perc 0.001 0.4) :action FREE)
     (+ (pulse (midicps (- note 12)))
        (sin-osc (midicps (- note 7))))
     amp))

(def my-overpad
  ;(partial my-pad :release 0.2 :amp 0.1)
  (partial overpad :release 0.2 :amp 0.0)
)

(def play-bass (partial play-with-controls #'my-bass *bass))

(defn play-all []
  (let [nome (metronome 128) beat (nome)]
  ;  (slicer-player nome beat)
    (sequencer nome beat *vocals 1/2 32)
    (player giorgio {} nome beat #'my-overpad 256 (* 4 256))
    (player around-arp-low {} nome beat play-arp-low 16 64)
    (player around-arp {} nome beat play-arp 16 64)
  ;  (player around-bass around-bass-ctrl nome beat play-bass 16 64)
    (player within {} nome beat #'play-bass 16 64)
    (sequencer nome beat *beats 1/4 4)
    beat))

(def *controls (atom {13 {:param :amp :min 0 :max 1.0}
                      11 {:param :filt :min 1 :max 24}
                       1 {:param :coef :min 0.1 :max 1.0}}))

(def *cut-controls (atom {13 {:param :amp :min 0 :max 1.0}
                          11 {:param :cutoff :min 0 :max 1.0}}))

(comment
  (play-all)
  (stop)
  (swap! *beats-flt assoc :cutoff 0.99)
  (swap! *beats assoc lpf-kick #{0 1 2 3})
  (swap! *beats assoc lpf-kick #{0 1 2 3 7/4 9/4})
  (swap! *beats assoc lpf-snare #{1 3})
  (swap! *beats assoc lpf-snare #{1 3 11/4 7/2})
  (swap! *beats assoc clap #{1 3})
  (untztrument play-arp *bass *cut-controls)
  (untztrument play-arp *beats-flt *beats-controls)
  (untztrument play-arp *arp-low *controls)
  (untztrument play-arp *arp *controls)
  (remove-event-handler ::untztrument-control)
  (swap! *beats assoc close-hat (into #{} (range 0 8 1/2)))
  (swap! *beats assoc lpf-open #{1/2 3/2 5/2 7/2 11/4 13/4 3/4})
  (swap! *beats assoc lpf-open #{11/4 7/2})
  (dosync
    (swap! *beats assoc lpf-kick #{} lpf-snare #{})
    (swap! *bass assoc :amp 0.0)
    (swap! *arp-low assoc :amp 0.5)
    (swap! *vocals clear-vals))
  (reset! *beats beats)
  (swap! *beats clear-vals)
  (reset! *vocals vocals)
  (swap! *vocals clear-vals)
  (swap! *bass assoc :amp 0.2)
  (swap! *arp assoc :amp 0)
  (swap! *arp-low assoc :amp 0)
  (swap! *arp-low assoc :amp 0.9)
  (swap! *arp-low assoc :amp 0.5 :filt 24 :decay 0.9 :coef 0.1)
  (swap! *arp assoc :amp 0.9 :filt 24 :decay 0.9 :coef 0.1)
  (swap! *arp-low assoc :amp 0.5 :filt 12 :decay 0.9 :coef 0.1)
  (swap! *arp assoc :amp 0.5 :filt 6 :decay 0.4 :coef 0.1)
  (comment
    (swap! *vocals assoc s-around1 #{3/2 (+ 16 3/2)} s-around2-0 #{9/2 (+ 16 9/2)})
    (swap! *vocals assoc s-around1 #{} s-around2-0 #{})
    (swap! *vocals assoc s-around1 #{(+ 8 3/2) (+ 24 3/2)} s-around2-0 #{(+ 8 9/2) (+ 24 9/2)})
    (swap! *vocals assoc s-work-it #{1} s-make-it #{3} s-do-it #{5} s-makes-us #{7}
                         s-harder #{17} s-better #{19} s-faster #{21} s-stronger #{23})
    (swap! *vocals assoc s-make-it #{1 5} s-harder #{3} s-better #{7})
    (swap! *vocals assoc s-work-it #{1} s-make-it #{3} s-do-it #{5} s-makes-us #{7})))
(stop)
