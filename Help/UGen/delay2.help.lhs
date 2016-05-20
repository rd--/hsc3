    > Sound.SC3.UGen.Help.viewSC3Help "Delay2"
    > Sound.SC3.UGen.DB.ugenSummary "Delay2"

> import Sound.SC3 {- hsc3 -}
>
> g_01 = let s = impulse AR 1 0 in s + (delay2 s)
