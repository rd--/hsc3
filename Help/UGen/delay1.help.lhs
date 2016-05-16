    Sound.SC3.UGen.Help.viewSC3Help "Delay1"
    Sound.SC3.UGen.DB.ugenSummary "Delay1"

> import Sound.SC3 {- hsc3 -}
>
> g_01 = let s = impulse AR 1 0 in s + (delay1 s)

left=original, right=subtract delayed from original

> g_02 = let z = dust 'α' AR 1000 in mce2 z (z - delay1 z)
