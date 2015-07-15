(ns overtone-workshop.player
  (:use [overtone.live]))

(defn clear-vals [coll]
  (into {} (for [[k v] coll] [k #{}])))

(defn to-vec [m]
  (interleave (keys m) (vals m)))

(defn play-with-controls [synth controls & args]
  (apply synth (concat (to-vec @controls) args)))

(defn play-step [step idx start chord step-ctrl synth-fn]
  (dorun (for [n chord]
    (apply-at (+ (* step idx) start) #(apply synth-fn (concat (to-vec step-ctrl) [:note (note n)])) []))))

(defn player [pattern pattern-ctrl nome beat synth-fn beats resolution]
  (let [t (nome beat)
        next-beat (+ beats beat)
        next-beat-at (nome next-beat)
        step (/ (- next-beat-at t) resolution)]
    (dorun (for [[idx chord] pattern]
      (play-step step idx t chord (get pattern-ctrl idx) synth-fn)))
    (apply-by next-beat-at player [pattern pattern-ctrl nome next-beat synth-fn beats resolution])))

(defn sequencer [nome beat live-patterns scale beats]
  (doseq [[sound pattern] @live-patterns]
     (when (contains? pattern (mod (dec beat) beats))
       (at (nome beat) (sound))))
  (let [next-beat (+ scale beat)]
    (apply-by (nome next-beat) sequencer [nome next-beat live-patterns scale beats])))

