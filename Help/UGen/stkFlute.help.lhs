    Sound.SC3.UGen.Help.viewSC3Help "StkFlute"
    Sound.SC3.UGen.DB.ugenSummary "StkFlute"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as External {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.RDU as RDU {- sc3-rdu -}

...not working...

> g_01 =
>   let freq = 440
>       jetDelay = 49
>       noisegain = 0.15
>       jetRatio = 0.32
>   in External.stkFlute AR freq jetDelay noisegain jetRatio

> g_02 =
>   let freq = 440
>       jetDelay = 49
>       noisegain = line KR 0.05 0.25 3 DoNothing -- def = 0.15
>       jetRatio = line KR 0.10 0.60 3 RemoveSynth -- def = 0.32
>   in External.stkFlute AR freq jetDelay noisegain jetRatio

<https://ccrma.stanford.edu/software/stk/classstk_1_1Flute.html>
