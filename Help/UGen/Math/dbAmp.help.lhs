dbAmp a

Convert decibels to linear amplitude.

> import Sound.SC3

> let { a = dbAmp (line KR (-6) (-40) 10 RemoveSynth)
>     ; o = fSinOsc AR 800 0 * a }
> in audition (out 0 o)
