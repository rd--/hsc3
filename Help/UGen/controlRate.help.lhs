> import Sound.SC3 {- hsc3 -}

play a sine tone at control rate, the reciprocal of controlDur

> g_01 =
>     let f = mce2 controlRate (recip controlDur)
>     in sinOsc AR f 0 * 0.1
