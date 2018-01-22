    Sound.SC3.UGen.Help.viewSC3Help "PulseDPW"
    Sound.SC3.UGen.DB.ugenSummary "PulseDPW"

> import Sound.SC3 {- hsc3 -}

> g_01 = pulseDPW AR (xLine KR 2000 20 10 DoNothing) 0.5 * 0.1

> g_02 = pulseDPW AR (mouseX KR 200 12000 Exponential 0.2) 0.5 * 0.2
