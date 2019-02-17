    Sound.SC3.UGen.Help.viewSC3Help "LorenzTrig"
    Sound.SC3.UGen.DB.ugenSummary "LorenzTrig"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> f_01 minfreq maxfreq s b =
>   let r = 28
>       h = 0.02
>       x0 = 0.090879182417163
>       y0 = 2.97077458055
>       z0 = 24.282041054363
>   in X.lorenzTrig AR minfreq maxfreq s r b h x0 y0 z0

> f_02 = f_01 11025 44100

> g_01 = f_02 10 2.6666667

Randomly modulate params

> g_02 = f_02 (lfNoise0 'α' KR 1 * 2 + 10) (lfNoise0 'β' KR 1 * 1.5 + 2)

as a frequency control

> g_03 =
>   let n = f_01 1 8 10 28
>   in sinOsc AR (decay n 1.0 * 800 + 900) 0 * 0.4
