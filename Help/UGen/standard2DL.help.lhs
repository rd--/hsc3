> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> def_k = 1.4
> def_x0 = 4.9789799812499
> def_y0 = 5.7473416156381

mouse-controlled param

> g_01 = standard2DL AR 11025 44100 (mouseX KR 0.9 4 Linear 0.2) def_x0 def_y0 * 0.3

as a frequency control

> g_02 =
>   let x = mouseX KR 0.9 4 Linear 0.2
>       f = standard2DL AR 10 20 x def_x0 def_y0 * 800 + 900
>   in sinOsc AR f 0 * 0.3
