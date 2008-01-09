dgeom length start grow

Demand rate geometric series ugen.

start	- start value
grow 	- value by which to grow ( x = x[-1] * grow )
length	- number of values to create

The arguments can be a number or any other ugen

> do { n <- dgeom 15 1 1.2
>    ; let { x = mouseX KR 1 40 Exponential 0.1
>          ; t = impulse KR x 0
>          ; f = demand t 0 n * 30 + 340 }
>      in audition (out 0 (sinOsc AR f 0 * 0.1)) }
