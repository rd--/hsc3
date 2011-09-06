> Sound.SC3.UGen.Help.viewSC3Help "PV_Copy"
> Sound.SC3.UGen.DB.ugenSummary "PV_Copy"

> import Sound.SC3.ID

> withSC3 (\fd -> do {_ <- async fd (b_alloc 0 2048 1)
>                    ;async fd (b_alloc 1 2048 1)})

Proof of concept, silence
> let {i = lfClipNoise 'a' AR 100 * 0.1
>     ;c0 = fft' 0 i
>     ;c1 = pv_Copy c0 1}
> in audition (out 0 (ifft' c0 - ifft' c1))
