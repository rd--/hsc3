twoPole in freq radius

Two pole filter.  A two pole filter. This provides lower level
access to setting of pole location.  For general purposes Resonz is
better.

> import Sound.SC3.Monadic

> do { n <- whiteNoise AR
>    ; audition (out 0 (twoPole (n * 0.005) 2000 0.95)) }

> do { n <- whiteNoise AR
>    ; let f = xLine KR 800 8000 8 RemoveSynth
>      in audition (out 0 (twoPole (n * 0.005) f 0.95)) }
