    Sound.SC3.UGen.Help.viewSC3Help "StkBowed"
    Sound.SC3.UGen.DB.ugenSummary "StkBowed"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

...not working...

> g_01 =
>   let g = toggleFF (impulse KR 1 0)
>       freq = 220.0
>       bowpressure = 64.0
>       bowposition = 64.0
>       vibfreq = 64.0
>       vibgain = 64.0
>       loudness_ = 64.0
>       gate_ = 1.0
>       attackrate = 1
>       decayrate = 1
>   in stkBowed AR freq bowpressure bowposition vibfreq vibgain loudness_ gate_ attackrate decayrate

<https://ccrma.stanford.edu/software/stk/classstk_1_1Bowed.html>
