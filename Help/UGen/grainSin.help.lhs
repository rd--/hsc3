    > Sound.SC3.UGen.Help.viewSC3Help "GrainSin"
    > Sound.SC3.UGen.DB.ugenSummary "GrainSin"

> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let n = whiteNoise 'α' KR
>         x = mouseX KR (-0.5) 0.5 Linear 0.1
>         y = mouseY KR 0 400 Linear 0.1
>         f = n * y + 440
>         t = impulse KR 10 0
>     in grainSin 2 t 0.1 f x (-1) 512 * 0.1
