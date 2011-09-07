> Sound.SC3.UGen.Help.viewSC3Help "Operator.round"
> :t roundUp

> import Sound.SC3

> let { x = mouseX' KR 60 4000 Linear 0.1
>     ; f = roundUp x 100 }
> in audition (out 0 (sinOsc ar f 0 * 0.1))

> let n = line KR 24 108 6 RemoveSynth
> in audition (out 0 (saw AR (midiCPS (roundUp n 1)) * 0.2))
