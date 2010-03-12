brownNoise rate

Generates noise whose spectrum falls off in power by 6 dB per
octave.

> import Sound.SC3.ID

> let n = brownNoise 'a' AR
> in audition (out 0 (n * 0.1))

> let { n = brownNoise 'a' KR
>     ; o = sinOsc AR (linExp n (-1) 1 64 9600) 0 * 0.1 }
> in audition (out 0 o)

