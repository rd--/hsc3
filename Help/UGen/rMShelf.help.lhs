> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

rm = regalia-mitra
freq = cut-off frequency (hz)
k = gain (db)

default parameters

> g_01 = X.rMShelf2 AR (whiteNoise 'α' AR * 0.1) 440 0

freq = mouse-x

> g_02 =
>   let freq = mouseX KR 55 3520 Exponential 0.2
>   in X.rMShelf2 AR (whiteNoise 'α' AR * 0.1) freq 0

k = mouse-y

> g_03 =
>   let freq = mouseX KR 55 3520 Exponential 0.2
>       k = mouseY KR (-12) 12 Linear 0.2
>   in X.rMShelf2 AR (whiteNoise 'α' AR * 0.1) freq k
