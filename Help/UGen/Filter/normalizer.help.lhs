normalizer in level dur

Flattens dynamics.

> let { s = fSinOsc AR 500 0
>     ; z = decay2 (impulse AR 8 (lfSaw KR 0.25 (-0.6) * 0.7)) 0.001 0.3 * s }
> in audition (out 0 (mce [z, normalizer z 0.4 0.01]))
