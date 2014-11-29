> Sound.SC3.UGen.Help.viewSC3Help "LFTri"
> Sound.SC3.UGen.DB.ugenSummary "LFTri"

> import Sound.SC3

> audition (out 0 (lfTri AR 500 1 * 0.1))

Used as both Oscillator and LFO.

> audition (out 0 (lfTri AR (lfTri KR 4 0 * 400 + 400) 0 * 0.1))

Multiple phases

> let f = lfTri KR 0.4 (mce [0..3]) * 200 + 400
> in audition (out 0 (mix (lfTri AR f 0 * 0.1)))

triangle wave as sum of sines.
for partial n, amplitude is (1 / square n) and phase is pi at every other odd partial

> import Sound.SC3.UGen.Dot

> let {mk_freq f0 n = f0 * fromInteger n
>     ;mk_amp n = if even n then 0 else 1 / fromInteger (n * n)
>     ;mk_ph n = if n + 1 `mod` 4 == 0 then pi else 0
>     ;mk_param f0 n =
>      let m = [1,3 .. n]
>      in zip3 (map (mk_freq f0) m) (map mk_ph m) (map mk_amp m)
>     ;x = midiCPS (mouseX KR 20 72 Linear 0.2)
>     ;e = xLine KR 0.01 1 20 DoNothing
>     ;o1 = sum (map (\(fr,ph,am) -> sinOsc AR fr ph * am) (mk_param x 25)) * (1 - e)
>     ;o2 = lfTri AR x 0 * e}
> in audition (out 0 (mce2 o1 o2 * 0.1))

drawings

> import Sound.SC3.Plot {- hsc3-plot -}

> plot_ugen1 0.1 (lfTri AR 40 0)
> plot_ugen1 0.1 (lfTri AR (xLine KR 1 800 0.1 DoNothing) 0)
