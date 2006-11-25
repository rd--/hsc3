twoPole in freq radius

Two pole filter.  A two pole filter. This provides lower level
access to setting of pole location.  For general purposes Resonz is
better.

> n <- whiteNoise AR
> audition $ twoPole (n * 0.005) 2000 0.95

> n <- whiteNoise AR
> let f = xLine KR 800 8000 8 RemoveSynth
> audition $ twoPole (n * 0.005) f 0.95
