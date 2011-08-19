latch in trig

Sample and hold. Holds input signal value when triggered.

in   - input signal.
trig - trigger. The trigger can be any signal. A trigger happens when the
       signal changes from non-positive to positive.

> import Sound.SC3.Monadic

> do { n <- whiteNoise AR
>    ; let { i = impulse AR 9 0
>          ; l = latch n i }
>      in audition (out 0 (blip AR (l * 400 + 500) 4 * 0.2)) }

The above is just meant as example. LFNoise0 is a faster way to
generate random steps :

> do { n <- lfNoise0 KR 9
>    ; audition (out 0 (blip AR (n * 400 + 500) 4 * 0.2)) }

http://create.ucsb.edu/pipermail/sc-users/2006-December/029991.html

> do { n0 <- lfNoise2 KR 8
>    ; n1 <- lfNoise2 KR 3
>    ; let { s = blip AR (n0 * 200 + 300) (n1 * 10 + 20)
>          ; x = mouseX' KR 1000 (sampleRate * 0.1) Exponential 0.1 }
>      in audition (out 0 (latch s (impulse AR x 0))) }
