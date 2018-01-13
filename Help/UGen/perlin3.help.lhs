    Sound.SC3.UGen.Help.viewSC3Help "Perlin3"
    Sound.SC3.UGen.DB.ugenSummary "Perlin3"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> g_01 =
>     let x = integrator (k2a (mouseX KR 0 0.1 Linear 0.2)) 1.0
>         y = integrator (k2a (mouseY KR 0 0.1 Linear 0.2)) 1.0
>     in perlin3 AR x y 0
