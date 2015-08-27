# overtone-workshop

Basic introduction to sound synthesis, rhythms and effects using [Overtone](https://github.com/overtone/overtone) and [Clojure](https://github.com/clojure/clojure).

## Interacting with code

* Install [lein](https://github.com/technomancy/leiningen)

* Install [jack](https://github.com/overtone/overtone/wiki/Installing-and-starting-jack) (linux only) 

* Run `lein repl` and wait for prompt.

* Inside REPL run

```clojure
(use 'overtone.live) ; this would take a while
(use 'overtone-workshop.letsgo)
(play-all (metronome 128))
(stop)
```

## Inspirations

* [Syntorial](http://www.syntorial.com/)
* [Calvin Harris "Lets Go" - Making The Beat](https://www.youtube.com/watch?v=wtGtnshXIU0)
* [Ellie Goulding "Lights" - Making The Beat](https://www.youtube.com/watch?v=A_TiZhgQ9Fw)
* [Daft Club](http://daft.club/daftabase/?lang=en)
* https://github.com/danieltwagner/random-access-memories

## License

Copyright © 2015 Piotr Jagielski

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
