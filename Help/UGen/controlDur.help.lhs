    > Sound.SC3.UGen.Help.viewSC3Help "ControlDur"
    > Sound.SC3.UGen.DB.ugenSummary "ControlDur"

> import Sound.SC3 {- hsc3 -}

controlRate and controlDur are reciprocals

> g_01 =
>    let f = mce2 controlRate (recip controlDur)
>    in sinOsc AR f 0 * 0.1
