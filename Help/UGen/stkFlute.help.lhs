    Sound.SC3.UGen.Help.viewSC3Help "StkFlute"
    Sound.SC3.UGen.DB.ugenSummary "StkFlute"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.HW.External.SC3_Plugins {- hsc3 -}

> g_01 =
>   let bp = line KR 76 32 3 RemoveSynth
>       ng = line KR 16 64 3 DoNothing
>   in stkFlute AR 400 64 ng 16 16 bp 1
