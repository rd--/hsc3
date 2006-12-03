dser length array

Demand rate sequence generator.

array  - array of values or other ugens
length - number of values to return

> a <- dser 8192 (MCE [1, 3, 2, 7, 8])
> let x = mouseX KR 1 40 Exponential 0.1
>     t = impulse KR x 0
>     f = demand t 0 a * 30 + 340
> audition $ sinOsc AR f 0 * 0.1
