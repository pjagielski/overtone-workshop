(ns overtone-workshop.patterns
  (:use [overtone.live]))

(def shrt {:sustain 0.1})

(def letsgo
  (merge
    (zipmap [0 3 6 9] (repeat 4 [:B6 :G6 :B5 :G5 :B4]))
    (zipmap [16 19 22 25] (repeat 4 [:E7 :B6 :G6 :E6 :B5 :G5 :B4]))
    (zipmap [32 35 38 41] (repeat 4 [:C7 :E6 :C6 :G5 :C5]))
    (zipmap [48 51] (repeat 2 [:F#7 :D7 :F#6 :D6 :A5 :D5]))
    (zipmap [54 57] (repeat 2 [:G7 :E7 :G6 :E6 :B5 :E5]))))

(def letsgo-bass
  {0  [:E2] 3  [:E2] 6  [:E2] 9  [:E2] 11 [:E3] 12 [:E2] 14 [:E3]
   16 [:G2] 19 [:G2] 22 [:G2] 25 [:G2] 27 [:G2] 30 [:G2]
   32 [:A2] 35 [:A2] 38 [:A2] 41 [:A2] 43 [:A2] 46 [:A2]
   48 [:B2] 51 [:B2] 54 [:C3] 57 [:C3] 59 [:C3] 62 [:C3]})

(def letsgo-bass-ctrl
  {11 shrt 12 shrt 14 shrt 27 shrt 30 shrt 43 shrt 46 shrt 57 shrt 59 shrt 62 shrt})

(defn repeat-line [idx line]
 (into {}
   (for [[k v] line]
     [(+ idx k) v])))

(def derezed-line
  {0 [:E4] 3 [:G4] 6 [:E4] 8 [:E5] 9 [:B4] 10 [:G4] 11 [:D4] 12 [:A4] 13 [:E4] 14 [:G4] 15 [:A4]})

(def derezed
  (conj
    (repeat-line 0 derezed-line)
    (repeat-line 16 derezed-line)))

(def lights
  {0 [:G#3 :B4] 3 [:B4] 6 [:G#3 :B4] 8 [:G#4] 11 [:D#4] 14 [:F#3 :B4] 16 [:A#4] 17 [:A#4] 19 [:A#4] 20 [:F#3] 22 [:F#4] 25 [:C#4] 28 [:F#4] 32 [:E3 :B4] 35 [:B4] 38 [:E3 :B4] 40 [:G#4] 43 [:E4] 46 [:C#3 :B4] 49 [:G#4] 52 [:E4] 54 [:C#3 :B4] 57 [:G#4] 60 [:E4]})

(def lights-bass
  {0 [:G#2] 14 [:F#2] 32 [:E2] 46 [:C#2]})

(def lights-bass-control
  {0  {:sustain 1.7 :release 0.3} 14 {:sustain 2.2 :release 0.3}
   32 {:sustain 1.7 :release 0.3} 46 {:sustain 2.2 :release 0.3}})

(def lights-strings
  {0 [:G#4 :B4 :D#4] 14 [:C#4 :F#4 :A#4] 32 [:E4 :G#4 :B4] 46 [:E4 :G#4 :B4]})

(defn shift-patt [shift patt]
  (map #(+ % shift) patt))

(def around-arp-low
  (let [patt [0 2 3 5 6 8 10 11 13 14]]
    (merge
      (zipmap (shift-patt 0 patt)  (repeat 10 [:C#2]))
      (zipmap (shift-patt 16 patt) (repeat 10 [:D#2]))
      (zipmap (shift-patt 32 patt) (repeat 10 [:F2]))
      (zipmap (shift-patt 48 patt) (repeat 10 [:F2])))))

(def around-arp
  {1  [:F3]  4 [:G3]  7 [:G#3]  9 [:F3] 12 [:G3] 15 [:G#3] 17 [:F3] 20 [:G3] 23 [:G#3] 25 [:F3] 28 [:G3] 31 [:G#3]
   33 [:F3] 36 [:G3] 39 [:G#3] 41 [:F3] 44 [:G3] 47 [:G#3] 49 [:F3] 52 [:G3] 55 [:G#3] 57 [:F3] 61 [:G3] 63 [:G#3]})

(def follow-bass
  (merge
    (zipmap [0  4  8  11 14] (repeat 5 [:F3]))
    (zipmap [16 20 24 27 30] (repeat 5 [:G3]))
    (zipmap [32 36 40 43 46] (repeat 5 [:A3]))
    (zipmap [48 52 56 59 62] (repeat 5 [:G3]))))

(def follow
  (merge
    (zipmap [0  4  10 14] (repeat 5 [:F3 :F4 :C5 :A4]))
    (zipmap [16 20 26 30] (repeat 5 [:G3 :G4 :B4 :D5]))
    (zipmap [32 36 42 46] (repeat 5 [:A3 :A4 :C5 :E5]))
    (zipmap [48 52 58 62] (repeat 5 [:G3 :G4 :B4 :E5]))))

(defn transpose [updown notes]
  (map (fn [n] (find-note-name (+ updown (note n)))) notes))

(def repetition-sub-a [:C5, :A3, :B4, :A3, :C5, :E5, :A3, :A4, :C5, :A3, :B4, :A3, :C5, :A4])
(def repetition-a (concat [:A4, :A3] repetition-sub-a [:A3, :A4] repetition-sub-a))

(def repetition-b [:F4, :F4, :A4, :F4, :G4, :F4, :A4, :C5, :F4, :F4, :A4, :F4, :G4, :F4, :A4, :F4])

;; slight variation of the above with different distances between the 2nd and 3rd note
(def repetition-b3 [:E4, :E4, :G4, :E4, :F#3, :E4, :G4, :B4, :E4, :E4, :G4, :E4, :F#3, :E4, :G4, :E4])

(def theme (concat
              repetition-a
              (transpose -5 repetition-a)
              repetition-a
              (transpose -5 repetition-a)
              repetition-b
              (transpose 2 repetition-b)
              (transpose -2 repetition-b3)
              repetition-b3
              repetition-b
              (transpose 2 repetition-b)
              repetition-b3
              repetition-b3))

(def short-theme
  (concat repetition-a
          (transpose -5 repetition-a)))

(def score (concat
             (concat (drop-last theme) [(note :A4)])
             theme
             (concat (drop-last theme) [(note :A4)])
             (concat (drop-last theme) [(note :A4)])))

(def giorgio
  (->>
    short-theme
    (zipmap (range (count short-theme)))
    (map (fn [[k v]] {k [v]}))
    (into {})))

(def within
  (let [patt [0 2 3 5 6 9 10 12 13 14]]
    (merge
      (zipmap (shift-patt 0 patt) (repeat 14 [:A#2]))
      {14 [:G#2] 15 [:G#2]}
      (zipmap (shift-patt 16 patt) (repeat 14 [:F#2]))
      {30 [:F2] 31 [:F2]}
      (zipmap (range 32 46) (repeat 14 [:D#2]))
      {46 [:F2] 47 [:F2]}
      (zipmap (range 48 58) (repeat 14 [:G#2]))
      (zipmap (range 58 64) (repeat 10 [:F2])))))

