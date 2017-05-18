    > Sound.SC3.UGen.Help.viewSC3Help "EnvFollow"
    > Sound.SC3.UGen.DB.ugenSummary "EnvFollow"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.HW.External.SC3_Plugins {- hsc3 -}

> g_01 =
>     let z = soundIn 4
>         d = mouseX KR 0.990 0.999 Linear 0.2
>         c = envFollow KR z d
>         o = pinkNoise 'α' AR * c
>     in mce2 z o
