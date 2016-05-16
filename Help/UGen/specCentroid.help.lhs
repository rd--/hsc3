> Sound.SC3.UGen.Help.viewSC3Help "SpecCentroid"
> Sound.SC3.UGen.DB.ugenSummary "SpecCentroid"

> import Sound.SC3

as the number of harmonics increases, the centroid is pushed higher
> let {f0 = mouseY KR 1000 100 Exponential 0.2
>     ;nh = mouseX KR 1 100 Exponential 0.2
>     ;z = blip AR f0 nh
>     ;f = fft' (localBuf 'α' 2048 1) z
>     ;c = specCentroid f
>     ;p = poll' (impulse KR 1 0) c (label "c") 0
>     ;o = sinOsc AR p 0 * 0.1}
> in audition (out 0 o)
