latch in trig

Sample and hold. Holds input signal value when triggered.

in   - input signal.
trig - trigger. The trigger can be any signal. A trigger happens when the
       signal changes from non-positive to positive.

> g = blip AR (l * 400 + 500) 4 * 0.2
>     where n = whitenoise' (UId 0) AR
>           i = impulse AR 9 0
>           l = latch n i

The above is just meant as example. LFNoise0 is a faster way to
generate random steps :

> blip AR (lfnoise0' (UId 0) KR 9 * 400 + 500) 4 * 0.2
