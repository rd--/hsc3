    Sound.SC3.UGen.Help.viewSC3Help "PV_BinShift"
    Sound.SC3.UGen.DB.ugenSummary "PV_BinShift"

> import Sound.SC3 {- hsc3 -}

source signal (oscillators)

> g_01 =
>   let s0 = sinOsc KR 0.08 0 * 6 + 6.2
>       s1 = sinOsc KR (squared s0) 0 * 100 + 800
>   in sinOsc AR s1 0 * 0.2

source signal (the world)

> g_02 = soundIn 0

default values

> f_01 z = ifft' (pv_BinShift (ffta 'α' 2048 z 0.5 0 1 0) 1 0 0)

> g_03 = f_01 g_02

mouse control

> f_02 z =
>   let x = mouseX KR (-10) 100 Linear 0.1
>       y = mouseY KR 1 4 Linear 0.1
>       b = mouseButton KR 0 1 0.2
>       pv = pv_BinShift (ffta 'β' 2048 z 0.5 0 1 0) y x b
>   in ifft' pv

> g_04 = f_02 g_02
