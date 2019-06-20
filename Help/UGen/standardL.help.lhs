> import Sound.SC3 {- hsc3 -}

> g_00 = standardL AR (sampleRate / 2) 1.0 0.5 0 * 0.2

vary frequency

> g_01 = standardL AR (mouseX KR 20 sampleRate Linear 0.2) 1 0.5 0 * 0.3

mouse-controlled param

> g_02 = standardL AR (sampleRate / 2) (mouseX KR 0.9 4 Linear 0.2) 0.5 0 * 0.3

as a frequency control

> g_03 = sinOsc AR (standardL AR 40 (mouseX KR 0.9 4 Linear 0.2) 0.5 0 * 800 + 900) 0 * 0.3
