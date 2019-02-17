    Sound.SC3.UGen.Help.viewSC3Help "NestedAllpassL"
    Sound.SC3.UGen.DB.ugenSummary "NestedAllpassL"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> nestedAllpassL_def s =
>   let d1 = 0.036
>       d2 = 0.030
>   in X.nestedAllpassL s d1 d1 0.08 d2 d2 0.3

> doubleNestedAllpassL_def s =
>   let d1 = 0.0047
>       d2 = 0.022
>       d3 = 0.0084
>   in X.doubleNestedAllpassL s d1 d1 0.15 d2 d2 0.25 d3 d3 0.3

> f_01 nc s =
>   let fb = localIn nc AR 0
>       lp0 = lpf s 6000
>       lp1 = delayL lp0 0.024 0.024
>       ap1 = doubleNestedAllpassL_def (lp1 + (0.5 * fb))
>       ap2 = nestedAllpassL_def ap1
>       revout = ap1 * 0.5 + ap2 * 0.6
>       locout = localOut (bpf (revout * 0.5) 1600 0.5)
>   in mrg2 revout locout

> f_02 nc s =
>   let fb = localIn nc AR 0
>       lp = lpf s 6000
>       ap1 = X.doubleNestedAllpassL (lp + (0.5 * fb)) 0.0047 0.0047 0.25 0.0083 0.0083 0.35 0.022 0.022 0.45
>       ap2 = delayL (X.nestedAllpassL (delayL ap1 0.05 0.05) 0.03 0.03 0.25952 0.03 0.03 0.3) 0.067 0.067
>       ap3 = X.nestedAllpassL (lp + (delayL ap2 0.015 0.015 * 0.4)) 0.0292 0.0292 0.25 0.0098 0.0098 0.35
>       revout = sum_opt [ap1,ap2,ap3] * 0.5
>       locout = localOut (bpf (revout * 0.4) 1000 0.5)
>   in mrg2 revout locout

> f_03 nc s =
>   let fb = localIn nc AR 0
>       lp = lpf s 4000
>       ap1 = allpassL (lp + (0.5 * fb)) 0.008 0.008 0.0459
>       ap2 = delayL (allpassL ap1 0.012 0.012 0.06885) 0.004 0.004
>       ap3 = delayL (X.nestedAllpassL (delayL ap2 0.017 0.017) 0.025 0.025 0.5 0.062 0.062 0.25) 0.031 0.031
>       ap4 = X.doubleNestedAllpassL (delayL ap3 0.003 0.003) 0.120 0.120 0.5 0.076 0.076 0.25 0.030 0.030 0.25
>       revout = sum_opt [ap4 * 0.8,ap3 * 0.8,ap2 * 1.5]
>       locout = localOut (bpf (revout * 0.4) 1000 0.5)
>   in mrg2 revout locout

> g_00 = soundIn 0

> g_01 =
>   let sig = soundIn 0
>       rev = f_01 2 sig
>   in 0.5 * rev + sig

> g_02 =
>   let sig = soundIn 0
>       rev = f_02 2 sig
>   in 0.5 * rev + sig

> g_03 =
>   let sig = soundIn 0
>       rev = f_03 2 sig
>   in 0.5 * rev + sig
