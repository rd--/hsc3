drand  length array
dxrand length array

Demand rate random sequence generators.

length	- number of values to return
array	- array of values or other ugens

Dxrand never plays the same value twice, whereas Drand chooses any
value in the list.

> n <- drand 32 (MCE [1,3,2,7,8])
> let x = mouseX kr 1 400 Exponential 0.1
>     t = impulse KR x 0
>     f = demand t 0 n * 30 + 340
> audition $ sinOsc AR f 0 * 0.1
