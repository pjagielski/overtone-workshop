(ns overtone-workshop.beat
  (:use [overtone.live]))

(def resolution 4)
(def nome (metronome (* 90 resolution)))

(def kick (freesound-sample 192248))
(def snare (freesound-sample 82583))
(def hat (freesound-sample 165027))

(def _ 0)
(def pats {kick  [1 _ _ _ _ _ _ 1 _ 1 1 _ _ 1 _ 1 1 _ _ _ _ _ _ 1 _ 1 1 _ 1 _ 1 _]
           snare [_ _ _ _ 1 _ _ _ _ _ _ _ 1 _ _ _ _ _ _ _ 1 _ _ _ _ _ _ _ _ _ _ _]
           hat   [_ _ _ _ _ _ 1 _ 1 _ 1 _ _ _ 1 _ 1 _ 1 _ 1 _ 1 _ _ _ _ _ _ _ _ _]})

(def live-pats (atom pats))

(defn live-perc-sequencer
  ([metro live-patterns]
   (let [cur-beat (metro)]
     (live-perc-sequencer metro live-patterns cur-beat cur-beat)))
  ([metro live-patterns beat start-beat]
   (doseq [[sound pattern] @live-patterns
           :when (= 1 (nth pattern (mod (- beat start-beat) (count pattern))))]
     (at (metro beat) (sound)))
   (let [next-beat (inc beat)]
     (apply-by (metro next-beat) live-perc-sequencer [metro live-patterns next-beat start-beat]))))

(comment
  (live-perc-sequencer nome live-pats))

(stop)
