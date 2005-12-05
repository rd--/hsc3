formlet in freq attackTime decayTime

FOF-like filter

> formlet AR (impulse AR 20 0.5) 1000 0.01 0.1

> formlet AR (blip AR (xline KR 10 400 8 2) 1000 * 0.1) 1000 0.01 0.1

Modulating formant frequency.

> let s = blip AR (sinosc KR 5 0 * 20 + 300) 1000 * 0.1
> in formlet AR s (xline KR 1500 700 8 2) 0.005 0.04
