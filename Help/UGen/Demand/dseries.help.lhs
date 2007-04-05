dseries length start step

Demand rate arithmetic series ugen.

length  - number of values to create
start   - start value
step    - step value

The arguments can be a number or any other ugen

> n <- dseries 15 0 1
> let x = mouseX KR 1 40 Exponential 0.1
>     t = impulse KR x 0
>     f = demand t 0 n * 30 + 340
> audition (out 0 (sinOsc AR f 0 * 0.1))
