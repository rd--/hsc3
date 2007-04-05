dust2 density

Generates random impulses from -1 to +1.  The `density' is in
impulses per second.

> n <- dust2 AR 200
> audition (out 0 (n * 0.5))

> let d = xLine KR 20000 2 10 RemoveSynth
> audition . (out 0 ) . (* 0.25) =<< dust2 AR d
