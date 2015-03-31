(ns overtone-workshop.patterns
  (:use [overtone.live]))

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
