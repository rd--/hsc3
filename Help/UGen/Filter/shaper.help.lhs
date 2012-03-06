> Sound.SC3.UGen.Help.viewSC3Help "Shaper"
> Sound.SC3.UGen.DB.ugenSummary "Shaper"

> import Sound.SC3

> let mk_b fd a = do {_ <- async fd (b_alloc 10 512 1)
>                    ;let f = [Normalise,Wavetable,Clear]
>                     in async fd (b_gen_cheby 10 f a)}

> let s = sinOsc AR 300 0 * line KR 0 1 6 RemoveSynth
> in withSC3 (\fd -> do {_ <- mk_b fd [1,0,1,1,0,1]
>                       ;play fd (out 0 (shaper 10 s * 0.1))})

> let s = sinOsc AR 400 (pi / 2) * line KR 0 1 6 RemoveSynth
> in withSC3 (\fd -> do {_ <- mk_b fd [0.25,0.5,0.25]
>                       ;play fd (out 0 (shaper 10 s * 0.1))})
