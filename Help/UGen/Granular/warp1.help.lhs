> Sound.SC3.UGen.Help.viewSC3Help "Warp1"
> Sound.SC3.UGen.DB.ugenSummary "Warp1"

> import Sound.SC3

> let {fn = "/home/rohan/data/audio/pf-c5.aif"
>     ;p = linLin (lfSaw KR 0.05 0) (-1) 1 0 1
>     ;x = mouseX KR 0.5 2 Linear 0.1
>     ;w = warp1 1 10 p x 0.1 (-1) 8 0.1 2}
> in withSC3 (do {send (b_allocRead 10 fn 0 0)
>                ;play (out 0 w)})

real-time (delayed) input
> let {r = recordBuf AR 10 0 1 0 1 Loop 1 DoNothing (soundIn 4)
>     ;ph = (8192 / sampleRate) * 2 * pi
>     ;p = lfSaw KR (1 / bufDur KR 10) ph * 0.5 + 0.5
>     ;x = mouseX KR 0.5 2 Linear 0.2
>     ;y = mouseY KR 0.01 0.2 Linear 0.2
>     ;w = warp1 1 10 p x 0.1 (-1) 8 y 4}
> in withSC3 (do {send (b_alloc 10 8192 1)
>                ;play (out 0 (mrg2 w r))})
