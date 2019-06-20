> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Record.Plain.StkBowed {- hsc3-rec -}

> g_01 = mkStkBowed stkBowedR

...gate prints however...

> g_02 =
>   let g = lfPulse KR 0.25 0 0.5
>       u = stkBowedR
>         {freq = midiCPS (rand 'α' 24 96)
>         ,bowpressure = rand 'β' 0 127
>         ,bowposition = rand 'γ' 0 127
>         ,vibfreq = rand 'δ' 32 96
>         ,vibgain = rand 'ε' 32 96
>         ,loudness_ = rand 'ζ' 32 96
>         ,gate_ = g
>         ,attackrate = rand 'η' 1 64
>         ,decayrate = rand 'θ' 1 64}
>   in mkStkBowed u

<https://ccrma.stanford.edu/software/stk/classstk_1_1Bowed.html>
