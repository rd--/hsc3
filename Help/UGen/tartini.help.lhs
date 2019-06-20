> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

Comparison of input frequency (x) and tracked oscillator frequency (f).

> g_01 =
>   let x = mouseX KR 440 880 Exponential 0.1
>       o = lfSaw AR x 0 * 0.05 {- sinOsc AR x 0 * 0.1 -}
>       [f,_e] = mceChannels (X.tartini KR o 0.2 2048 0 1024 0.5)
>       r = sinOsc AR f 0 * 0.1
>       t = impulse KR 4 0
>       pf = poll t f 0 (label "f")
>       px = poll t x 0 (label "x")
>   in mrg [out 0 (mce2 o r),pf,px]

Fast test of live pitch tracking, not careful with amplitude of input

> g_02 =
>   let z = hpf (soundIn 0) 90
>       [f,e] = mceChannels (X.tartini KR z 0.2 2048 0 1024 0.5)
>   in mce2 (z * 0.1) (lfTri AR f 0 * 0.2 * lag e 0.2 * lag (f >** 90 * f <** 500) 0.2)

Printer for pitch tracker

> g_03 =
>   let i = soundIn 0
>       [f,_e] = mceChannels (X.tartini KR i 0.2 2048 0 1024 0.5)
>       r = sinOsc AR f 0 * 0.1
>       t = impulse KR 4 0
>       pf = poll t f 0 (label "f")
>   in mrg [out 0 (mce2 i r),pf]

ZELL = C4 - C5 = 247 259 277 295 309 331 345 369 387 415 441 462 493 (hz)
