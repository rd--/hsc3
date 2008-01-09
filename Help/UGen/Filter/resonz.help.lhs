resonz in freq bwr

Resonant filter.

A two pole resonant filter with zeroes at z = +/- 1. Based on
K. Steiglitz, "A Note on Constant-Gain Digital Resonators,"
Computer Music Journal, vol 18, no. 4, pp. 8-10, Winter 1994.  The
reciprocal of Q is used rather than Q because it saves a divide
operation inside the unit generator.

in - input signal to be processed
freq - resonant frequency in Hertz
rq - bandwidth ratio (reciprocal of Q). rq = bandwidth / centerFreq

> do { n <- whiteNoise AR
>    ; audition (out 0 (resonz (n * 0.5) 2000 0.1)) }

Modulate frequency

> do { n <- whiteNoise AR
>    ; let f = xLine KR 1000 8000 10 RemoveSynth
>      in audition (out 0 (resonz (n * 0.5) f 0.05)) }

Modulate bandwidth

> do { n <- whiteNoise AR
>    ; let bw = xLine KR 1 0.001 8 RemoveSynth
>      in audition (out 0 (resonz (n * 0.5) 2000 bw)) }

Modulate bandwidth opposite direction

> do { n <- whiteNoise AR
>    ; let bw = xLine KR 0.001 1 8 RemoveSynth
>      in audition (out 0 (resonz (n * 0.5) 2000 bw)) }
