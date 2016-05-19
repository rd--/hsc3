    > Sound.SC3.UGen.Help.viewSC3Help "Dswitch"
    > Sound.SC3.UGen.DB.ugenSummary "Dswitch"

> import Sound.SC3 {- hsc3 -}
>
> mk_g :: UId m => (UGen -> UGen -> m UGen) -> m UGen
> mk_g sw = do
>   a0 <- dwhiteM 2 3 4
>   a1 <- dwhiteM 2 0 1
>   a2 <- dseqM 2 (mce [1,1,1,0])
>   i <- dseqM 2 (mce [0,1,2,1,0])
>   d <- sw i (mce [a0,a1,a2])
>   let t = impulse KR 4 0
>       f = demand t 0 d * 300 + 400
>   return (sinOsc AR f 0 * 0.1)

compare with dswitch1

> g_01,g_02 :: UId m => m UGen
> g_01 = mk_g dswitchM
> g_02 = mk_g dswitch1M
