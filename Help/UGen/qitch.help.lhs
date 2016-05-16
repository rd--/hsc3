> Sound.SC3.UGen.Help.viewSC3Help "Qitch"
> Sound.SC3.UGen.DB.ugenSummary "Qitch"

> import Sound.SC3
> import System.FilePath

Load required data file (edit path as required)

> let {d = "/home/rohan/opt/share/SuperCollider/Extensions/SC3plugins"
>     ;qf = d </> "PitchDetection/extraqitchfiles/QspeckernN2048SR48000.wav"}
> in withSC3 (async (b_allocRead 10 qf 0 0))

Comparison of input frequency (x) and tracked oscillator frequency (f).
Output is printed to the console by scsynth.

> let {x = mouseX KR 440 880 Exponential 0.1
>     ;o = sinOsc AR x 0 * 0.1
>     ;[f,e] = mceChannels (qitch KR o 10 1e-2 1 0 0 2500)
>     ;r = sinOsc AR f 0 * 0.1
>     ;t = impulse KR 4 0
>     ;pf = poll t f (label "f") 0
>     ;px = poll t x (label "x") 0}
> in audition (mrg [out 0 (mce2 o r),pf,px])
