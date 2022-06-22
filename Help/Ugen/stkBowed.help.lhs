> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Record.Plain.StkBowed {- hsc3-rec -}

> g_01 = mkStkBowed stkBowedR

...gate prints however...

> g_02 =
>   let g = lfPulse kr 0.25 0 0.5
>       u = stkBowedR
>         {freq = midiCps (randId 'α' 24 96)
>         ,bowpressure = randId 'β' 0 127
>         ,bowposition = randId 'γ' 0 127
>         ,vibfreq = randId 'δ' 32 96
>         ,vibgain = randId 'ε' 32 96
>         ,loudness_ = randId 'ζ' 32 96
>         ,gate_ = g
>         ,attackrate = randId 'η' 1 64
>         ,decayrate = randId 'θ' 1 64}
>   in mkStkBowed u

<https://ccrma.stanford.edu/software/stk/classstk_1_1Bowed.html>
