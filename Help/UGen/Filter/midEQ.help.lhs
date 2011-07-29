midEQ i freq rq db

Parametric filter.  Attenuates or boosts a frequency band.

i = The input signal.
freq = Center frequency of the band in Hertz.
rq = The reciprocal of Q (bandwidth / cutoffFreq).
db = Amount of boost (db > 0) or attenuation (db < 0) of the frequency band.

> import Sound.SC3.ID

> let f = midiCPS (fSinOsc KR 1 0 * 24 + 84)
> in audition (out 0 (midEQ (saw AR 200 * 0.2) f 0.3 12))

> let { i = pinkNoise 'a' AR * 0.2 + sinOsc AR 600 0 * 0.1
>     ; f = sinOsc KR 0.2 (0.5 * pi) * 2 + 600 }
> in audition (out 0 (midEQ i f 0.01 (-24)))
