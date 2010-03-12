dbrown  length lo hi step
dibrown length lo hi step

Demand rate brownian movement generators.

lo              - minimum value
hi              - maximum value
step            - maximum step for each new value
length          - number of values to create

Dbrown returns numbers in the continuous range between lo and hi,
Dibrown returns integer values.  The arguments can be a number or
any other ugen.

> import Sound.SC3.Monadic

> do { n <- dbrown dinf 0 15 1
>    ; let { x = mouseX KR 1 40 Exponential 0.1
>          ; t = impulse KR x 0
>          ; f = demand t 0 n * 30 + 340 }
>      in audition (out 0 (sinOsc AR f 0 * 0.1)) }
