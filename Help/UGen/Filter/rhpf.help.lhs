rhpf in freq rq

A resonant high pass filter.

> let f = fSinOsc KR (xLine KR 0.7 300 20 RemoveSynth) 0 * 3600 + 4000
> audition $ rhpf (saw AR 200 * 0.1) f 0.2
