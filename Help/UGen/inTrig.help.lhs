    > Sound.SC3.UGen.Help.viewSC3Help "InTrig"
    > Sound.SC3.UGen.DB.ugenSummary "InTrig"

> import Sound.SC3 {- hsc3 -}

Run an oscillator with the trigger at bus 10.

> g_01 =
>     let t = inTrig 1 10
>         e = envGen KR t t 0 1 DoNothing (envPerc 0.01 1)
>     in sinOsc AR 440 0 * e

Set bus 10, each set will trigger a ping.

    > withSC3 (send (c_set1 10 0.1))
