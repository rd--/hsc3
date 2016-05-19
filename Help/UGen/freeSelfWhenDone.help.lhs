    > Sound.SC3.UGen.Help.viewSC3Help "FreeSelfWhenDone"
    > Sound.SC3.UGen.DB.ugenSummary "FreeSelfWhenDone"

> import Sound.SC3 {- hsc3 -}

using RemoveSynth doneAction

> g_01 =
>     let x = mouseX KR (-1) 1 Linear 0.1
>         e = linen x 1 0.1 1 RemoveSynth
>     in sinOsc AR 440 0 * e

using FreeSelfWhenDone UGen

> g_02 =
>     let x = mouseX KR (-1) 1 Linear 0.1
>         e = linen x 1 0.1 1 DoNothing
>     in mrg [sinOsc AR 440 0 * e,freeSelfWhenDone e]
