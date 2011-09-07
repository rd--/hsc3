> Sound.SC3.UGen.Help.viewSC3Help "InTrig"
> Sound.SC3.UGen.DB.ugenSummary "InTrig"

# hsc3
channel count (Int) is first argument

> import Sound.SC3

Run an oscillator with the trigger at bus 10.
> let {t = inTrig 1 10
>     ;e = envGen KR t t 0 1 DoNothing (envPerc 0.01 1)}
> in audition (out 0 (sinOsc AR 440 0 * e))

Set bus 10, each set will trigger a ping.
> withSC3 (\fd -> send fd (c_set1 10 0.1))
