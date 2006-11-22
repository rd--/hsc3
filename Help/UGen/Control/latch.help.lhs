latch in trig

Sample and hold. Holds input signal value when triggered.

in   - input signal.
trig - trigger. The trigger can be any signal. A trigger happens when the
       signal changes from non-positive to positive.

> n <- whiteNoise AR
> let i = impulse AR 9 0
> let l = latch n i
> audition $ blip AR (l * 400 + 500) 4 * 0.2

The above is just meant as example. LFNoise0 is a faster way to
generate random steps :

> n <- lfNoise0 KR 9
> audition $ blip AR (n * 400 + 500) 4 * 0.2
