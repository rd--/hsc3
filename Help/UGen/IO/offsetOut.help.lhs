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

> let a = do
>       {sr <- serverSampleRateActual
>       ;t <- utcr
>       ;let {b = control KR "bus" 0
>            ;g = synthdef "g" (offsetOut b (sinOsc AR 440 0 * 0.2))
>            ;m i = s_new "g" (-1) AddToTail 1 [("bus",i)]
>            ;p i = Bundle (UTCr (t + 1.0)) [m i]
>            ;q i = Bundle (UTCr (t + 1.1)) [m i]
>            ;r i = Bundle (UTCr (t + 1.0 + sr/1000)) [m i]}
>       ;_ <- async (d_recv g)
>       ;mapM_ sendBundle [p 0,p 1,q 0,r 1]}
> in withSC3 a
