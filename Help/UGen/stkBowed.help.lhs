    Sound.SC3.UGen.Help.viewSC3Help "StkBowed"
    Sound.SC3.UGen.DB.ugenSummary "StkBowed"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.HW.External.SC3_Plugins {- hsc3 -}

no longer working...

> g_01 =
>   let g = toggleFF (impulse KR 1 0)
>   in stkBowed AR 220 64 64 64 64 64 g 1 1
