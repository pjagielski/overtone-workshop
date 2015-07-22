(ns overtone-workshop.patterns
  (:use [overtone.live]))

(def shrt {:sustain 0.1})

(def letsgo {0  [:B6 :G6 :B5 :G5 :B4]
             3  [:B6 :G6 :B5 :G5 :B4]
             6  [:B6 :G6 :B5 :G5 :B4]
             9  [:B6 :G6 :B5 :G5 :B4]
             16 [:E7 :B6 :G6 :E6 :B5 :G5 :B4]
             19 [:E7 :B6 :G6 :E6 :B5 :G5 :B4]
             22 [:E7 :B6 :G6 :E6 :B5 :G5 :B4]
             25 [:E7 :B6 :G6 :E6 :B5 :G5 :B4]
             32 [:C7 :E6 :C6 :G5 :C5]
             35 [:C7 :E6 :C6 :G5 :C5]
             38 [:C7 :E6 :C6 :G5 :C5]
             41 [:C7 :E6 :C6 :G5 :C5]
             48 [:F#7 :D7 :F#6 :D6 :A5 :D5]
             51 [:F#7 :D7 :F#6 :D6 :A5 :D5]
             54 [:G7 :E7 :G6 :E6 :B5 :E5]
             57 [:G7 :E7 :G6 :E6 :B5 :E5]})

(def letsgo-bass
  {0  [:E2] 3  [:E2] 6  [:E2] 9  [:E2] 11 [:E3] 12 [:E2] 14 [:E3]
   16 [:G2] 19 [:G2] 22 [:G2] 25 [:G2] 27 [:G2] 30 [:G2]
   32 [:A2] 35 [:A2] 38 [:A2] 41 [:A2] 43 [:A2] 46 [:A2]
   48 [:B2] 51 [:B2] 54 [:C3] 57 [:C3] 59 [:C3] 62 [:C3]})

(def letsgo-bass-ctrl
  {11 shrt 12 shrt 14 shrt 27 shrt 30 shrt 43 shrt 46 shrt 57 shrt 59 shrt 62 shrt})

(def animals {
   0 [:G#5 :C#5 :F4] 4 [:G#5 :C#5 :F4] 6 [:A#5 :C5 :F4] 8 [:C6 :D#5 :G#4]
   12 [:D#6 :D#5 :G#4] 15 [:A#5 :G4] 18 [:A#5 :G4] 20 [:G#5 :A#4] 22 [:G5 :G#4] 24 [:F5 :C5 :F4]
   32 [:G5 :C5 :F4] 34 [:G5 :C5 :F4] 36 [:G#5 :C5 :F4] 38 [:G#5 :C5 :F4]
   40 [:F5 :C5 :F4] 46 [:G5 :C5 :F4] 50 [:G5 :C5 :F4]
   52 [:G#5 :C5 :F4] 54 [:G#5 :C5 :F4] 56 [:F5 :C5 :F4]})

(def ratherbe-main
 {0 [:G#3 :B4] 1 [:D#5] 2 [:G#5] 3 [:G#3 :B4 :B5]
  6 [:G#3 :C#5 :D#5 :G#5 :C#6] 8 [:F#3 :A#4 :D#5 :F#5 :A#5]})

(def ratherbe-sec
 {0 [:E3 :G#4] 1 [:B4] 2 [:E5] 3 [:E3 :G#4 :B4 :G#5]
  6 [:E3 :G#4 :B4 :E5 :F#5] 8 [:D#3 :F#4 :B4 :D#5] 12 [:D#3 :F#4 :B4 :D#5 :F#5]})

(def ratherbe-end1
 {0 [:E3 :G#4 :G#5] 1 [:D#5] 2 [:B4] 3 [:E3 :G#4] 5 [:G#5]
  6 [:E3 :G#4 :D#5] 7 [:B4] 8 [:C#3 :E4]})

(def ratherbe-end2
 {0 [:C#4 :E4] 1 [:G#4] 2 [:B4] 3 [:C#4 :E4 :D#5] 4 [:F#5] 5 [:G#5]
  6 [:C#4 :E4] 7 [:B5] 8 [:E4 :G#4] 9 [:C#6] 11 [:F#4 :A#4 :D#6] 14 [:G#4 :B4 :F#6]})

(defn repeat-line [idx line]
 (into {}
   (for [[k v] line]
     [(+ idx k) v])))

(def ratherbe
 (conj (repeat-line 0 ratherbe-main)
       (repeat-line 16 ratherbe-sec)
       (repeat-line 32 ratherbe-main)
       (repeat-line 48 ratherbe-sec)
       (repeat-line 64 ratherbe-main)
       (repeat-line 80 ratherbe-sec)
       (repeat-line 96 ratherbe-end1)
       (repeat-line 112 ratherbe-end2)))

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

(def one
  {0 [:D4 :A4] 3 [:G4 :D4] 6 [:D4 :F4] 9 [:E4 :D4] 12 [:F4 :D4] 15 [:D4 :F4] 18 [:D4 :F4] 21 [:F4 :D4]
   24 [:D4 :F4] 28 [:D4 :F4] 30 [:D4 :F4] 32 [:A4 :D4] 35 [:G4 :D4] 38 [:F4 :D4] 41 [:E4 :D4] 44 [:G4 :D4]
   47 [:G4 :D4] 50 [:G4 :D4] 53 [:G4 :D4] 56 [:G4 :D4] 60 [:G4 :D4] 62 [:G4 :D4]})

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

(def around-bass
  {0 [:A1] 4 [:A1] 8 [:A1] 12 [:A1] 14 [:B1] 15 [:C2] 20 [:C2] 24 [:C2] 28 [:C2] 30 [:D2] 31 [:E2] 36 [:E2] 40 [:E2] 44 [:E2] 48 [:F#2] 50 [:E2] 52 [:D2] 54 [:B1] 56 [:B1] 58 [:A1] 60 [:G1]})

(def around-bass-ctrl
  {48 {:sustain 0.05} 50 {:sustain 0.05} 52 {:sustain 0.05} 54 shrt 56 shrt 58 shrt 60 {:sustain 0.3}})

(def dafunk
  {0 [:G4] 8 [:F4] 10 [:G4] 12 [:A#4] 14 [:D4] 24 [:C4] 26 [:D4] 28 [:F4] 30 [:A#3]
   40 [:A3] 42 [:A#3] 44 [:D4] 46 [:G3] 56 [:A3] 60 [:A#3]})

(def dafunk-ctrls
  {0 {:sustain 0.7} 14 {:sustain 0.8} 30 {:sustain 0.8} 46 {:sustain 0.8} 56 {:sustain 0.4} 60 {:sustain 0.4}})

(def danse
  {0 [:C4] 3 [:F4] 6 [:C5] 10 [:C5] 14 [:C5] 16 [:G#4] 19 [:C#5] 22 [:G4] 26 [:G4] 30 [:G4]})

(def follow-chorus
  (merge
    (zipmap [0  4  8  11 14] (repeat 5 [:F3 :F4 :C5 :A4]))
    (zipmap [16 20 24 27 30] (repeat 5 [:G3 :G4 :B4 :D5]))
    (zipmap [32 36 40 43 46] (repeat 5 [:A3 :A4 :C5 :E5]))
    (zipmap [48 52 56 59 62] (repeat 5 [:G3 :G4 :B4 :E5]))))

(def follow-bass
  (merge
    (zipmap [0  4  10 14] (repeat 5 [:F3]))
    (zipmap [16 20 26 30] (repeat 5 [:G3]))
    (zipmap [32 36 46] (repeat 5 [:A3]))
    (zipmap [48 52 58 62] (repeat 5 [:G3]))))

(def follow
  (merge
    (zipmap [0  4  10 14] (repeat 5 [:F3 :F4 :C5 :A4]))
    (zipmap [16 20 26 30] (repeat 5 [:G3 :G4 :B4 :D5]))
    (zipmap [32 36 42 46] (repeat 5 [:A3 :A4 :C5 :E5]))
    (zipmap [48 52 58 62] (repeat 5 [:G3 :G4 :B4 :E5]))))

(def doit
  {0 [:C3] 1 [:C3] 2 [:A4] 4 [:A4] 6 [:A3] 7 [:A3] 8 [:A4]})

