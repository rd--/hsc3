    Sound.SC3.UGen.Help.viewSC3Help "MulAdd"
    Sound.SC3.UGen.DB.ugenSummary "MulAdd"

> import Sound.SC3 {- hsc3 -}

> g_01 = mulAdd (sinOsc AR 440 0) 0.1 0.05

These should both optimise to the same graph...

> g_02 = sinOsc AR 440 0 * 0.1 + 0.05

> g_03 = 0.05 + sinOsc AR 440 0 * 0.1
