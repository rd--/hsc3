> Sound.SC3.UGen.Help.viewSC3Help "Warp1"
> Sound.SC3.UGen.DB.ugenSummary "Warp1"

> import Sound.SC3

> let {fn = "/home/rohan/data/audio/pf-c5.aif"
>     ;p = linLin (lfSaw KR 0.05 0) (-1) 1 0 1
>     ;x = mouseX' KR 0.5 2 Linear 0.1
>     ;w = warp1 1 10 p x 0.1 (-1) 8 0.1 2}
> in withSC3 (\fd -> do {send fd (b_allocRead 10 fn 0 0)
>                       ;play fd (out 0 w)})

