    Sound.SC3.UGen.Help.viewSC3Help "Coyote"
    Sound.SC3.UGen.DB.ugenSummary "Coyote"

> import Sound.SC3
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> g_01 =
>   let i = soundIn 0
>       c = coyote KR i 0.2 0.2 0.01 0.5 0.05 0.1
>       o = pinkNoise 'α' AR * decay c 1
>   in mce2 i o
