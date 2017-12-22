    Sound.SC3.UGen.Help.viewSC3Help "DiodeRingMod"
    Sound.SC3.UGen.DB.ugenSummary "DiodeRingMod"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> o_01 = fSinOsc AR 800 0
> o_02 = fSinOsc AR (xLine KR 200 500 5 DoNothing) 0
> g_01 = diodeRingMod o_01 o_02 * 0.125
> g_02 = ((o_01 * o_02) + o_01) * 0.125

> g_03 =
>   let s1 = sinOsc AR (3700 * mce [1, 1.1, 1.2] * range 1 2 (sinOsc AR 200 0)) 0
>       s2 = sinOsc AR (100 * mce [0.75, 1, 0.5]) 0
>       s3 = diodeRingMod s1 s2
>   in mix s3 * 0.2 * lfPulse AR (10.3 * 0.5) 0 0.04 * 0.1
