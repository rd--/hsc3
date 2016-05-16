> Sound.SC3.UGen.Help.viewSC3Help "LFSaw"
> Sound.SC3.UGen.DB.ugenSummary "LFSaw"

# SC2 did not have the initial phase argument.

> import Sound.SC3

> audition (out 0 (lfSaw AR 500 1 * 0.1))

Used as both Oscillator and LFO.

> audition (out 0 (lfSaw AR (lfSaw KR 4 0 * 400 + 400) 0 * 0.1))

Output range is bi-polar.

> let {f = mce [linLin (lfSaw KR 0.5 0) (-1) 1 200 1600, 200, 1600]
>     ;a = mce [0.1,0.05,0.05]}
> in audition (out 0 (mix (sinOsc AR f 0 * a)))

saw-tooth wave as sum of sines.
for all partials n, amplitude is (1 / n).
phase is always 0.
cross-fade from sum to lfSaw.

> let {mk_freq f0 n = f0 * fromInteger n
>     ;mk_amp n = 1 / fromInteger n
>     ;mk_param f0 n = let m = [1,2 .. n] in zip (map (mk_freq f0) m) (map mk_amp m)
>     ;x = midiCPS (mouseX KR 20 72 Linear 0.2)
>     ;y = mouseY KR 0.01 0.1 Exponential 0.2
>     ;e = xLine KR 0.01 1 20 DoNothing
>     ;o1 = sum (map (\(fr,am) -> sinOsc AR fr 0 * am) (mk_param x 25)) * (1 - e)
>     ;o2 = lfSaw AR x 0 * e}
> in audition (out 0 (mce2 o1 o2 * y))

> import Sound.SC3.Plot {- hsc3-plot -}

> plot_ugen1 0.1 (lfSaw AR 50 0) -- ascending
> plot_ugen1 0.002 (lfSaw AR 5000 0)

