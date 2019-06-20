> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Record.Plain.StkPluck {- hsc3-rec -}

> g_01 = mkStkPluck stkPluckR

> g_02 =
>   let u = stkPluckR
>           {freq = midiCPS (iRand 'α' 32 96)
>           ,decay_ = rand 'β' 0 127}
>       s = leakDC (mkStkPluck u) 0.995
>   in mrg2 s (detectSilence s 0.01 0.5 RemoveSynth)
