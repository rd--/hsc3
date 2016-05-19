    > Sound.SC3.UGen.Help.viewSC3Help "ControlRate"
    > Sound.SC3.UGen.DB.ugenSummary "ControlRate"

> import Sound.SC3 {- hsc3 -}

play a sine tone at control rate, the reciprocal of controlDur

> g_01 =
>     let f = mce2 controlRate (recip controlDur)
>     in sinOsc AR f 0 * 0.1
