ringz in freq decayTime

Ringing filter.  This is the same as Resonz, except that instead of
a resonance parameter, the bandwidth is specified in a 60dB ring
decay time. One Ringz is equivalent to one component of the Klank
UGen.

> n <- dust AR 3
> audition $ ringz (n * 0.3) 2000 2

> n <- whiteNoise AR
> audition $ ringz (n * 0.005) 2000 0.5

Modulate frequency

> n <- whiteNoise AR
> audition $ ringz (n * 0.005) (xLine KR 100 3000 10 RemoveSynth) 0.5

> let f = xLine KR 100 3000 10 RemoveSynth
> audition $ ringz (impulse AR 6 0.3) f 0.5

Modulate ring time

> let rt = xLine KR 4 0.04 8 RemoveSynth
> audition $ ringz (impulse AR 6 0.3) 2000 rt

Modulate ring time opposite direction

> let rt = xLine KR 0.04 4 8 RemoveSynth
> audition $ ringz (impulse AR 6 0.3) 2000 rt
