(ns overtone-workshop.sampler
  (:use [overtone.live]))

(defsynth sampler [in 0 rate 1 amp 0.95 pos 0 out-bus 0]
  (let [output (play-buf 1 in rate :start-pos pos :action FREE)]
    (out out-bus (* amp (pan2 output)))))

(defsynth slicer [in 0 start 0 end 44100 fade 0.01 mul 1 amp 0.7 rate 1]
  (let [phase (phasor:ar 0 (* rate (buf-rate-scale:kr in)) start end)
        sig (buf-rd:ar 2 in phase 0)
        dur (/ (- end start)
               (- (buf-sample-rate:kr in) (* 2 fade)))
        env (env-gen (env-lin fade dur fade mul) :action FREE)]
    (out 0 (pan2 (* sig env amp)))))
