> Sound.SC3.UGen.Help.viewSC3Help "OffsetOut"
> Sound.SC3.UGen.DB.ugenSummary "OffsetOut"

> import Sound.SC3

> let {a = offsetOut 0 (impulse AR 5 0)
>     ;b = out 0 (sinOsc AR 60 0 * 0.1)}
> in audition (mrg [a,b])

> let {a = out 0 (impulse AR 5 0)
>     ;b = out 0 (sinOsc AR 60 0 * 0.1) }
> in audition (mrg [a,b])

> import Sound.OSC

Phase cancellation, the 'offsetOut' at bus 0 cancels, the 'out'
at bus 1 doesn't (or at least is exceedingly unlikely to).
> let a = do
>       {t <- utcr
>       ;sr <- serverSampleRateActual
>       ;let {f = sr / 100
>            ;c = 1 / f
>            ;g = let o = sinOsc AR (constant f) 0 * 0.2
>                 in synthdef "g" (mrg [offsetOut 0 o,out 1 o])
>            ;m = s_new "g" (-1) AddToTail 1 []
>            ;p = Bundle (UTCr (t + 0.1)) [m]
>            ;q = Bundle (UTCr (t + 0.1 + c/2)) [m]}
>       ;_ <- async (d_recv g)
>       ;mapM_ sendBundle [p,q]}
> in withSC3 a
