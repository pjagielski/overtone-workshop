(ns overtone-workshop.timing
  (use [overtone.live]))

;; samples
(def kick (freesound-sample 192248))
(def snare (freesound-sample 82583))
;(def hat (freesound-sample 165027))
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

(defn player [nome beat]
  (let [next-beat (inc beat)]
    (at (nome beat) (kick))
    (apply-by (nome next-beat) player [nome next-beat])))

(comment
  (player nome (nome))
  (nome :bpm 160)
  (nome :bpm 240)
  (nome :bpm 540)
  (stop))

(def _ 0)
(def pats {kick  [1 _ _ _ 1 _ _ _]
           snare [_ _ 1 _ _ _ 1 _]
           hat   [_ _ _ _ _ _ _ _]})

(def live-pats (atom pats))

(defn live-sequencer
  [nome live-patterns scale idx beat]
  (doseq [[sound pattern] @live-patterns]
     (when (= 1 (nth pattern (mod idx (count pattern))))
       (at (nome beat) (sound))))
  (let [next-beat (+ scale beat)]
    (apply-by (nome next-beat) live-sequencer [nome live-patterns scale (inc idx) next-beat])))

(comment
  (live-sequencer nome live-pats 1/2 0 (nome))
  (swap! live-pats assoc kick [1 _ _ 1 _ 1 _ 1])
  (swap! live-pats assoc hat [_ 1 _ 1 _ 1 _ 1])
  (reset! live-pats pats)
  (stop))

(comment
  (nome :bpm 90)
  (reset! live-pats {kick  [1 _ _ _ _ _ _ 1 _ 1 1 _ _ 1 _ 1 1 _ _ _ _ _ _ 1 _ 1 1 _ 1 _ 1 _]
                     snare [_ _ _ _ 1 _ _ _ _ _ _ _ 1 _ _ _ _ _ _ _ 1 _ _ _ _ _ _ _ _ _ _ _]
                     hat   [_ _ _ _ _ _ 1 _ 1 _ 1 _ _ _ 1 _ 1 _ 1 _ _ _ _ _ _ _ _ _ _ _ _ _]})
  (live-sequencer nome live-pats 1/4 0 (nome))
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
  (let [time (now) beat (nome)]
    (at (nome) (ctl d :wobble 1.5 :note 29))
    (at (nome (+ 2 beat)) (ctl d :wobble 4 :note 40))
    (at (nome (+ 4 beat)) (ctl d :wobble 6 :note 45))
    (apply-by (nome (+ 6 beat)) skrillex [d nome])))

(comment
  (def d (dubstep 120))
  (ctl d :wobble 4)
  (ctl d :wobble 8)
  (ctl d :wobble 2 :note 31)
  (skrillex (dubstep (nome :bpm)) nome)
  (skrillex (dubstep) nome)
  (stop))

