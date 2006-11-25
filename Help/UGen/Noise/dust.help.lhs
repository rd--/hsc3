dust density

Generates random impulses from 0 to +1 at a rate determined by the
density argument.

> audition . (* 0.25) =<< dust AR 200

> let d = xLine KR 20000 2 10 RemoveSynth
> audition . (* 0.25) =<< dust AR d
