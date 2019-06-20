> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

Load required data file (edit path as required)

    > import System.FilePath {- filepath -}
    > d = "/home/rohan/opt/share/SuperCollider/Extensions/SC3plugins"
    > d = "/usr/share/SuperCollider/Extensions/SC3plugins"
    > qf = d </> "PitchDetection/extraqitchfiles/QspeckernN2048SR48000.wav"
    > withSC3 (async (b_allocRead 10 qf 0 0))

Comparison of input frequency (x) and tracked oscillator frequency (f).
Output is printed to the console by scsynth.

> g_01 =
>   let x = mouseX KR 440 880 Exponential 0.1
>       o = sinOsc AR x 0 * 0.1
>       [f,_e] = mceChannels (X.qitch KR o 10 1e-2 1 0 0 2500)
>       r = sinOsc AR f 0 * 0.1
>       t = impulse KR 4 0
>       pf = poll t f 0 (label "f")
>       px = poll t x 0 (label "x")
>   in mrg [out 0 (mce2 o r),pf,px]

> g_02 =
>   let s = hpf (soundIn 0) 110
>       [f,e] = mceChannels (X.qitch KR s 10 0.01 1 0 0 2500)
>       a = amplitude KR s 0.15 0.25
>       r = sinOsc AR f 0 * a * e
>   in out 0 (mce2 (s * 0.1) r)
