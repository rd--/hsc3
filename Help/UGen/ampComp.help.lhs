    > Sound.SC3.UGen.Help.viewSC3Help "AmpComp"
    > Sound.SC3.UGen.DB.ugenSummary "AmpComp"

> import Sound.SC3 {- hsc3 -}

mouse X axis frequency control

> x_freq = mouseX KR 300 1500 Exponential 0.2

> ampComp' :: Bool -> Rate -> UGen -> UGen -> UGen -> UGen
> ampComp' use_comp r fr rt ex = if use_comp then ampComp r fr rt ex else 1

> f_01 use_comp = sinOsc AR x_freq 0 * 0.1 * ampComp' use_comp KR x_freq 300 0.3333

without amplitude compensation

> g_01 = f_01 False

with amplitude compensation

> g_02 = f_01 True

modified exponent

> g_03 = pulse AR x_freq 0.5 * 0.1 * ampComp KR x_freq 300 1.3

in frequency modulation

> f_02 use_comp =
>     let freq = x_freq * sinOsc AR (mouseY KR 3 200 Exponential 0.2) 0 * 0.5 + 1
>     in sinOsc AR freq 0 * 0.1 * ampComp' use_comp KR freq 300 0.3333

with amplitude compensation

> g_04 = f_02 True

without amplitude compensation

> g_05 = f_02 False
