formant rate fundFreq formFreq bwFreq

Formant oscillator. Generates a set of harmonics around a formant
frequency at a given fundamental frequency.

Modulate fundamental frequency, formant frequency stays constant.

> audition (out 0 (formant AR (xLine KR 400 1000 8 RemoveSynth) 2000 800 * 0.125))

Modulate formant frequency, fundamental frequency stays constant.

> let f = mce [200, 300, 400, 500]
> in audition (out 0 (formant AR f (xLine KR 400 4000 8 RemoveSynth) 200 * 0.125))

Modulate width frequency, other frequencies stay constant.

> audition (out 0 (formant AR 400 2000 (xLine KR 800 8000 8 RemoveSynth) * 0.1))
