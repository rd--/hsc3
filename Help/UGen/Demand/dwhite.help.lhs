dwhite  length lo hi
diwhite length lo hi

Demand rate white noise random generators.

length  - number of values to create
lo      - minimum value
hi      - maximum value

Dwhite returns numbers in the continuous range between lo and hi,
Diwhite returns integer values.  The arguments can be a number or
any other ugen

> import Sound.SC3.Monadic

> do { n <- dwhite dinf 0 15
>    ; let { x = mouseX KR 1 40 Exponential 0.1
>          ; t = impulse KR x 0
>          ; f = demand t 0 n * 30 + 340 }
>      in audition (out 0 (sinOsc AR f 0 * 0.1)) }
