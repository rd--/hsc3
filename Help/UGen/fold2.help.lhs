> import Sound.SC3 {- hsc3 -}

Sound.SC3.Plot.plot_fn_r1_ln (\x -> fold2 x 1) (-2,2)

> g_01 = (fSinOsc AR 1000 0 `fold2` line KR 0 1 8 DoNothing) * 0.1
