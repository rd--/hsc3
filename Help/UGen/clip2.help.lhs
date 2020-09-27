> import Sound.SC3 {- hsc3 -}

Sound.SC3.Plot.plot_fn_r1_ln (\x -> clip2 x 1) (-2,2)

> g_01 = clip2 (fSinOsc AR 400 0) 0.2 -- clipping distortion
> g_02 = clip2 (fSinOsc AR 400 0) (line KR 0 1 8 RemoveSynth) * 0.1
