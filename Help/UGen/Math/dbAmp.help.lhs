# unaryop

> import Sound.SC3

Linear db motion is exponential amplitude decay
> let {a = dbAmp (line KR (-6) (-40) 10 RemoveSynth)
>     ;o = fSinOsc AR 800 0 * a}
> in audition (out 0 o)

There is a non-UGen variant.
> dbAmp (-26::Double)
