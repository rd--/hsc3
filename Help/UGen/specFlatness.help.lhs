    Sound.SC3.UGen.Help.viewSC3Help "SpecFlatness"
    Sound.SC3.UGen.DB.ugenSummary "SpecFlatness"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> g_01 =
>   let z = soundIn 0
>       g = 1 {- gain, set as required -}
>       a = poll' 1 (wAmp KR z 0.05) 0 (label "a")
>       f = fft' (localBuf 'α' 2048 1) z
>       c = poll' 1 (specCentroid KR f) 0 (label "c")
>       w = poll' 1 (specFlatness KR f) 0 (label "w")
>   in bpf (pinkNoise 'a' AR) c w * a * g
