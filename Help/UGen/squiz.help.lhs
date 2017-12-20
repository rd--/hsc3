    Sound.SC3.UGen.Help.viewSC3Help "Squiz"
    Sound.SC3.UGen.DB.ugenSummary "Squiz"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

Squiz of sin oscillator

> g_01 =
>   let o = sinOsc AR 440 0
>       x = mouseX KR 1 10 Exponential 0.2
>       y = mouseY KR 1 10 Linear 0.2
>   in squiz o x y 0.1 * 0.1

Load sound file to buffer zero

    > let fn = "/home/rohan/data/audio/pf-c5.aif"
    > let fn = "/home/rohan/opt/src/supercollider/sounds/a11wlk01.wav"
    > withSC3 (async (b_allocRead 0 fn 0 0))

Squiz of audio file.

> g_02 =
>   let r = bufRateScale KR 0
>       p = playBuf 1 AR 0 (r * 0.5) 1 0 Loop DoNothing
>       x = mouseX KR 1 100 Exponential 0.2
>       y = mouseY KR 1 10 Linear 0.2
>   in squiz p x y 0.1 * 0.5
