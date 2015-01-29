> Sound.SC3.UGen.Help.viewSC3Help "SpecFlatness"
> Sound.SC3.UGen.DB.ugenSummary "SpecFlatness"

> import Sound.SC3

> let {z = soundIn 4
>     ;g = 1 {- gain, set as required -}
>     ;a = wAmp KR z 0.05
>     ;f = fft' (localBuf 'α' 2048 1) z
>     ;c = poll' 1 (specCentroid f) (label "c") 0
>     ;w = poll' 1 (specFlatness f) (label "w") 0
>     ;o = bpf (pinkNoise 'a' AR) c w * a * g}
> in audition (out 0 o)
