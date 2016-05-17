    Sound.SC3.UGen.Help.viewSC3Help "LFPulse"
    Sound.SC3.UGen.DB.ugenSummary "LFPulse"

Note: SC2 had no initial phase argument.

> import Sound.SC3
>
> g_01 = let f = lfPulse KR 3 0 0.3 * 200 + 200 in lfPulse AR f 0 0.2 * 0.1
>
> g_02 = let x = mouseX KR 0 1 Linear 0.2 in lfPulse AR 220 0 x * 0.1

square wave as sum of sines.
for odd partials n, amplitude is (1 / n), for even partials amplitude is 0.
phase is always 0.

> g_03 =
>     let mk_freq f0 n = f0 * fromInteger n
>         mk_amp n = if even n then 0 else 1 / fromInteger n
>         mk_param f0 n = let m = [1,3 .. n] in zip (map (mk_freq f0) m) (map mk_amp m)
>         x = midiCPS (mouseX KR 20 72 Linear 0.2)
>         y = mouseY KR 0.01 0.1 Exponential 0.2
>         e = xLine KR 0.01 1 20 DoNothing
>         o1 = sum (map (\(fr,am) -> sinOsc AR fr 0 * am) (mk_param x 50)) * (1 - e)
>         o2 = lfPulse AR x 0 0.5 * e
>     in mce2 o1 o2 * y

Drawings

    > import Sound.SC3.Plot {- hsc3-plot -}
    > plot_ugen1 0.1 (lfPulse AR (line KR 100 800 0.1 DoNothing) 0 0.5)

![](sw/hsc3/Help/SVG/lfPulse.0.svg)
