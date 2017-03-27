    > Sound.SC3.UGen.Help.viewSC3Help "AmpComp"
    > Sound.SC3.UGen.DB.ugenSummary "AmpComp"

> import Sound.SC3 {- hsc3 -}

mouse X axis frequency control

> x_freq = mouseX KR 300 1500 Exponential 0.2

without compensation

> g_01 = sinOsc AR x_freq 0 * 0.1

with amplitude compensation

> g_02 = sinOsc AR x_freq 0 * 0.1 * ampComp AR x_freq 300 0.3333

modified exponent

> g_03 = pulse AR x_freq 0.5 * 0.1 * ampComp AR x_freq 300 1.3

in frequency modulation

> g_04 =
>     let freq = x_freq * sinOsc AR (mouseY KR 3 200 Exponential 0.2) 0 * 0.5 + 1
>     in sinOsc AR freq 0 * 0.1 * ampComp AR freq 300 0.3333
