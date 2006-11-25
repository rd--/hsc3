lpf in freq

Second order Butterworth lowpass filter.

> let f = xLine KR 0.7 300 20 RemoveSynth
> audition $ lpf (saw AR 200 * 0.1) (fSinOsc KR f 0 * 3600 + 4000)
