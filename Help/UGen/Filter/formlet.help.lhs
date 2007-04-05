formlet in freq attackTime decayTime

FOF-like filter

> audition (out 0 (formlet (impulse AR 20 0.5) 1000 0.01 0.1))

> let f = xLine KR 10 400 8 RemoveSynth
> audition (out 0 (formlet (blip AR f 1000 * 0.1) 1000 0.01 0.1))

Modulating formant frequency.

> let s = blip AR (sinOsc KR 5 0 * 20 + 300) 1000 * 0.1
> audition (out 0 (formlet s (xLine KR 1500 700 8 RemoveSynth) 0.005 0.04))
