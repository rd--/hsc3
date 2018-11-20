    Sound.SC3.UGen.Help.viewSC3Help "DNoiseRing"
    Sound.SC3.UGen.DB.ugenSummary "DNoiseRing"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as External {- hsc3 -}

> g_01 =
>   let tr = impulse AR 10 0
>       x = mouseX KR 0 1 Linear 0.2
>       y = mouseY KR 0 1 Linear 0.2
>       nr = demand tr 0 (External.dNoiseRing x y 1.0 32.0 0.0)
>       freq = midiCPS (linLin nr 0 (2**32) 40 (40 + 48))
>   in sinOsc AR freq 0 * 0.1
