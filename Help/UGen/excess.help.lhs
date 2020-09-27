> import Sound.SC3 {- hsc3 -}

Sound.SC3.Plot.plot_fn_r1_ln (\x -> excess x 1) (-2,2)

> g_01 = (fSinOsc AR 1000 0 `excess` line KR 0 1 8 DoNothing) * 0.1

or written out in terms of clip2

> g_02 =
>     let o = fSinOsc AR 1000 0
>         l = line KR 0 1 8 DoNothing
>     in (o - clip2 o l) * 0.1
