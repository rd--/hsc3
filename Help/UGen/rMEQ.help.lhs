    Sound.SC3.UGen.Help.viewSC3Help "RMEQ"
    Sound.SC3.UGen.DB.ugenSummary "RMEQ"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

rm = regalia-mitra
freq = cut-off frequency (hz)
rq = reciprocal-of-Q
k = gain (db)

default parameters

> g_01 = X.rMEQ AR (pinkNoise 'α' AR * 0.1) 440 1 0

> g_02 =
>   let freq = mouseX KR 55 3520 Exponential 0.2
>       rq = mouseY KR 0.01 2.0 Linear 0.2
>       k = 3
>   in X.rMEQ AR (pinkNoise 'α' AR * 0.1) freq rq k
