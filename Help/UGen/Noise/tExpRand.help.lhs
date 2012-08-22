> Sound.SC3.UGen.Help.viewSC3Help "TExpRand"
> Sound.SC3.UGen.DB.ugenSummary "TExpRand"

> import Sound.SC3.Monad

> do { f <- tExpRand 300.0 3000.0 =<< dust KR 10
>    ; audition (out 0 (sinOsc AR f 0 * 0.1)) }
