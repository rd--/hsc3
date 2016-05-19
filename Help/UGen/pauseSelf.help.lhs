    > Sound.SC3.UGen.Help.viewSC3Help "PauseSelf"
    > Sound.SC3.UGen.DB.ugenSummary "PauseSelf"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let x = mouseX KR (-1) 1 Linear 0.1
>         o = sinOsc AR 440 0 * 0.1
>     in mrg [o,pauseSelf x]

Run paused node (assuming no intermediate node is created).

    > withSC3 (send (n_run [(-1, True)]))
