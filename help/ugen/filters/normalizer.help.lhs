normalizer in level dur

Flattens dynamics.

> let s = fsinosc AR 500 0
>     z = decay2 AR (impulse AR 8 (lfsaw KR 0.25 (-0.6) s 0.7)) 0.001 0.3 
> in MCE [z, normalizer AR z 0.4 0.01]
