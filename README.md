# overtone-workshop

Basic introduction to sound synthesis, rhythms and effects using [Overtone](https://github.com/overtone/overtone) and [Clojure](https://github.com/clojure/clojure).

## Prerequisites

* Install [lein](https://github.com/technomancy/leiningen#installation)
* For Linux - install [jack](https://github.com/overtone/overtone/wiki/Installing-and-starting-jack)

## Interacting with code

### REPL

* Run `lein repl` and wait for prompt
* Inside REPL run
```clojure
(use 'overtone.live) ; this would take a while
(use 'overtone-workshop.letsgo)
(play-all (metronome 128))
(stop)
```

### Intellij IDEA

* Install [Cusive Clojure](https://cursiveclojure.com/userguide)
* File -> Open -> overtone-workshop
* Run -> Edit Configurations
* Add -> Clojure REPL -> Local, name: overtone-workshop
* Run -> Run overtone-workshop
* Open `sounds.clj`
* Load file in REPL (Shift-Control-L) 
* Wait for Overtone to load
* Point cursor to expression and Send to REPL (Shift-Control-P)
* Profit!

### Vim
 * Install [vim-fireplace](https://github.com/tpope/vim-fireplace)
 * Run `lein repl` and wait for prompt
 * In anothen terminal open `sounds.clj` in Vim
 * Run `:Eval` inside Vim
 * Point to start of expression and run `cpp`
 * Enjoy!

## Inspirations

* [Syntorial](http://www.syntorial.com/)
* [Calvin Harris "Lets Go" - Making The Beat](https://www.youtube.com/watch?v=wtGtnshXIU0)
* [Ellie Goulding "Lights" - Making The Beat](https://www.youtube.com/watch?v=A_TiZhgQ9Fw)
* [Daft Club](http://daft.club/daftabase/?lang=en)
* https://github.com/danieltwagner/random-access-memories

## License

Copyright © 2015 Piotr Jagielski

Samples © Daft Punk and respective owners

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
