    Sound.SC3.UGen.Help.viewSC3Help "BlitB3Square"
    Sound.SC3.UGen.DB.ugenSummary "BlitB3Square"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> g_01 = blitB3 AR (xLine KR 10000 20 10 DoNothing) * 0.2

spot the aliasing

> g_02 = impulse AR (xLine KR 10000 20 10 DoNothing) 0 * 0.2

sawtooth

> g_03 =
>   let x = mouseX KR 20 1000 Exponential 0.2
>   in leakDC (integrator (blitB3 AR x * 0.2) 0.99) 0.995

sawtooth; super-saw, can integrate mix
leaks dealt with one by one so don't accumulate

> g_04 =
>   let x = mouseX KR 1 4 Linear 0.2
>   in mix (leakDC (integrator (blitB3 AR (x * mce [220,221,223,224]) * 0.125) 0.99) 0.995)
