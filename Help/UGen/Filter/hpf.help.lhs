hpf in freq

Second order Butterworth highpass filter.

> let f = fSinOsc KR (xLine KR 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
> in audition (out 0 (hpf (saw AR 200 * 0.2) f))
