> Sound.SC3.UGen.Help.viewSC3Help "CoinGate"
> Sound.SC3.UGen.DB.ugenSummary "CoinGate"

> import Sound.SC3.ID

> let {g = coinGate 'a' 0.2 (impulse KR 10 0)
>     ;f = tRand 'b' 300.0 400.0 g}
> in audition (out 0 (sinOsc AR f 0 * 0.1))
