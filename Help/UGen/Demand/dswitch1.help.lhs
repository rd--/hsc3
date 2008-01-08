dswitch1 index array

Demand rate generator for switching between inputs

index	- which of the inputs to return
array	- array of values or other ugens

> let x = mouseX KR 0 4 Linear 0.1
>     y = mouseY KR 1 15 Linear 0.1
> w <- dwhite 2 0 3
> n <- dswitch1 x (mce [1, 3, y, 2, w])
> let t = impulse KR 3 0
>     f = demand t 0 n * 30 + 340
> audition (out 0 (sinOsc AR f 0 * 0.1))
