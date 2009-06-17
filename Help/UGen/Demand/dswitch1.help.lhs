dswitch1 index array

Demand rate generator for switching between inputs

index	- which of the inputs to return
array	- array of values or other ugens

> import Sound.SC3

> let { x = mouseX KR 0 4 Linear 0.1
>     ; y = mouseY KR 1 15 Linear 0.1
>     ; t = impulse KR 3 0 }
> in do { w <- dwhite dinf 20 23
>       ; n <- dswitch1 x (mce [1, 3, y, 2, w])
>       ; let f = demand t 0 n * 30 + 340
>         in audition (out 0 (sinOsc AR f 0 * 0.1)) }
