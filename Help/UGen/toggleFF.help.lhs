    > Sound.SC3.UGen.Help.viewSC3Help "ToggleFF"
    > Sound.SC3.UGen.DB.ugenSummary "ToggleFF"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let tr = dust 'α' AR (xLine KR 1 1000 60 DoNothing)
>         fr = toggleFF tr * 400 + 800
>     in sinOsc AR fr 0 * 0.1
