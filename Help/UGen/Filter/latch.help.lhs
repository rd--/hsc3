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

http://create.ucsb.edu/pipermail/sc-users/2006-December/029991.html

> n0 <- lfNoise2 KR 8
> n1 <- lfNoise2 KR 3
> let s = blip AR (n0 * 200 + 300) (n1 * 10 + 20)
>     x = mouseX KR 1000 (sampleRate * 0.1) Exponential 0.1
> audition $ latch s (impulse AR x 0)
