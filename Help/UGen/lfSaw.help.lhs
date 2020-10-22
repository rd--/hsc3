Note: SC2 did not have the initial phase argument.

> import Sound.SC3 {- hsc3 -}

> g_01 = lfSaw AR 500 1 * 0.1

Used as both Oscillator and LFO.

> g_02 = lfSaw AR (lfSaw KR 4 0 * 400 + 400) 0 * 0.1

Output range is bi-polar.

> g_03 =
>     let f = mce [linLin (lfSaw KR 0.5 0) (-1) 1 200 1600, 200, 1600]
>         a = mce [0.1,0.05,0.05]
>     in mix (sinOsc AR f 0 * a)

saw-tooth wave as sum of sines.
for all partials n, amplitude is (1 / n).
phase is always 0.
cross-fade from sum to lfSaw.

> g_04 =
>     let mk_freq f0 n = f0 * fromInteger n
>         mk_amp n = 1 / fromInteger n
>         mk_param f0 n = let m = [1,2 .. n] in zip (map (mk_freq f0) m) (map mk_amp m)
>         x = midiCPS (mouseX KR 20 72 Linear 0.2)
>         y = mouseY KR 0.01 0.1 Exponential 0.2
>         e = xLine KR 0.01 1 20 DoNothing
>         o1 = sum (map (\(fr,am) -> sinOsc AR fr 0 * am) (mk_param x 25)) * (1 - e)
>         o2 = lfSaw AR x 0 * e
>     in mce2 o1 o2 * y

as phasor input to sin function

> g_05 = sin (range 0 two_pi (lfSaw AR 440 0)) * 0.2

mixed with sin, then with distortions

> g_06 =
>   let f = xLine KR 220 440 10 DoNothing
>       o1 = sinOsc AR (f + mce2 0 0.7) 0
>       o2 = lfSaw AR (f + mce2 0 0.7) 0 * 0.3
>       o3 = o1 + o2
>       o4 = cubed (distort (log (distort o3)))
>   in o4 * 0.1

Drawings

    > import Sound.SC3.Plot {- hsc3-plot -}
    > plot_ugen1 0.1 (lfSaw AR 50 0) -- ascending
    > plot_ugen1 0.002 (lfSaw AR 5000 0)

![](sw/hsc3/Help/SVG/lfSaw.0.svg)
![](sw/hsc3/Help/SVG/lfSaw.1.svg)
