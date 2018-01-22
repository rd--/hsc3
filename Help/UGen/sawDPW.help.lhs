    Sound.SC3.UGen.Help.viewSC3Help "SawDPW"
    Sound.SC3.UGen.DB.ugenSummary "SawDPW"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> g_01 = sawDPW AR (xLine KR 2000 20 10 DoNothing) 0 * 0.1

> g_02 = sawDPW AR (mouseX KR 200 12000 Exponential 0.2) 0 * 0.2
