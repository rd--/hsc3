    Sound.SC3.UGen.Help.viewSC3Help "DPW4Saw"
    Sound.SC3.UGen.DB.ugenSummary "DPW4Saw"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> import qualified Sound.SC3.UGen.Bindings.DB.RDU as RDU {- sc3-rdu -}

> g_01 = X.dpw4Saw AR (xLine KR 2000 20 10 DoNothing) * 0.1

> g_02 = X.dpw4Saw AR (mouseX KR 200 12000 Exponential 0.2) * 0.2

> g_03 = saw AR (mouseX KR 200 12000 Exponential 0.2) * 0.1
