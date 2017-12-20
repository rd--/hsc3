    Sound.SC3.UGen.Help.viewSC3Help "BMoog"
    Sound.SC3.UGen.DB.ugenSummary "BMoog"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> f_01 md =
>   let dup x = mce2 x x
>       x = mouseX KR 20 12000 Exponential 0.2
>       o = dup (lfSaw AR (mce2 (x * 0.99) (x * 1.01)) 0 * 0.1)
>       cf = sinOsc KR (sinOsc KR 0.1 0) (1.5 * pi) * 1550 + 1800
>       y = mouseY KR 1 0 Linear 0.2
>       sig = bMoog o cf y md 0.95
>   in (combN sig 0.5 (mce2 0.4 0.35) 2 * 0.4) + (sig * 0.5)

modes are: 0 = lowpass, 1 = highpass, 2 = bandpass

> g_01 = f_01 0

> g_02 = f_01 1

> g_03 = f_01 2

> g_04 = f_01 (lfSaw KR 1 0 * 3)
