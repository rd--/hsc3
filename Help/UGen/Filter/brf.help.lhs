brf in freq rq

Second order Butterworth band reject filter.

> let f = fSinOsc KR (xLine KR 0.7 300 20 RemoveSynth) 0 * 3800 + 4000
> in audition (out 0 (brf (saw AR 200 * 0.1) f 0.3))
