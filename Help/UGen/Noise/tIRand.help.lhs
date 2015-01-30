> Sound.SC3.UGen.Help.viewSC3Help "TIRand"
> Sound.SC3.UGen.DB.ugenSummary "TIRand"

> import Sound.SC3

> do {l <- tIRandM (-1) 1 =<< dustM KR 10
>    ;n <- pinkNoiseM AR
>    ;audition (out 0 (pan2 (n * 0.1) l 1))}

> do {n <- tIRandM 4 12 =<< dustM KR 10
>    ;let f = n * 150 + (mce [0,1])
>     in audition (out 0 (sinOsc AR f 0 * 0.1))}
