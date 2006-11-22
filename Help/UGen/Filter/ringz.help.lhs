ringz in freq decayTime

Ringing filter.  This is the same as Resonz, except that instead of
a resonance parameter, the bandwidth is specified in a 60dB ring
decay time. One Ringz is equivalent to one component of the Klank
UGen.

> ringz AR (dust 0 AR 3 * 0.3) 2000 2

> ringz AR (whitenoise 0 AR * 0.005) 2000 0.5

Modulate frequency

> ringz AR (whitenoise 0 AR * 0.005) (xline KR 100 3000 10 2) 0.5

> ringz AR (impulse AR 6 0.3) (xline KR 100 3000 10 2) 0.5

Modulate ring time

> ringz AR (impulse AR 6 0.3) 2000 (xline KR 4 0.04 8 2)

Modulate ring time opposite direction

> ringz AR (impulse AR 6 0.3) 2000 (xline KR 0.04 4 8 2)
