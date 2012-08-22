> Sound.SC3.UGen.Help.viewSC3Help "PitchShift"
> Sound.SC3.UGen.DB.ugenSummary "PitchShift"

> import Sound.SC3

> let {r = mouseX KR 0.5 2.0 Linear 0.1
>     ;d = mouseY KR 0.0 0.1 Linear 0.1}
> in audition (out 0 (pitchShift (sinOsc AR 440 0) 0.2 r d 0))
