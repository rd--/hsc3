> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> g_01 = X.sawDPW AR (xLine KR 2000 20 10 DoNothing) 0 * 0.1

> g_02 = X.sawDPW AR (mouseX KR 200 12000 Exponential 0.2) 0 * 0.2
