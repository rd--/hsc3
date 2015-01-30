> Sound.SC3.UGen.Help.viewSC3Help "Hasher"
> Sound.SC3.UGen.DB.ugenSummary "Hasher"

> import Sound.SC3

noise

> audition (out 0 (hasher (line AR 0 1 1 RemoveSynth) * 0.2))

remap x

> let {x = mouseX KR 0 10 Linear 0.2
>     ;f = hasher (roundTo x 1) * 300 + 500}
> in audition (out 0 (sinOsc AR f 0 * 0.1))
