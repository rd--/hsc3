> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> f_01 zmax s =
>   let x = mouseX KR 1 10 Exponential 0.2
>       y = mouseY KR 1 zmax Linear 0.2
>   in X.squiz s x y 0.1 * 0.1

Squiz of sin oscillator

> g_01 = f_01 10 (sinOsc AR 440 0)


> g_02 = f_01 100 (soundIn 0)

Load sound file to buffer zero

    > let fn = "/home/rohan/data/audio/pf-c5.aif"
    > let fn = "/home/rohan/opt/src/supercollider/sounds/a11wlk01.wav"
    > withSC3 (async (b_allocRead 0 fn 0 0))

Squiz of audio file.

> g_03 =
>   let r = bufRateScale KR 0
>       p = playBuf 1 AR 0 (r * 0.5) 1 0 Loop DoNothing
>   in f_01 100 p
