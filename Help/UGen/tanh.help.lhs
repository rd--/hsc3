> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let e = xLine KR 0.1 10 10 DoNothing
>         o = fSinOsc AR 500 0.0
>     in tanh (o * e) * 0.25

Drawings:

    Sound.SC3.Plot.plot_fn_r1_ln tanh (-4,4)
