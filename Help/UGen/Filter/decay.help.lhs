decay in decayTime

Exponential decay.  This is essentially the same as Integrator
except that instead of supplying the coefficient directly, it is
caculated from a 60 dB decay time. This is the time required for
the integrator to lose 99.9 % of its value or -60dB. This is useful
for exponential decaying envelopes triggered by impulses.

Used as an envelope.

> import Sound.SC3.ID

> let { n = pinkNoise 'a' AR
>     ; s = impulse AR (xLine KR 1 50 20 RemoveSynth) 0.25 }
> in audition (out 0 (decay s 0.2 * n))
