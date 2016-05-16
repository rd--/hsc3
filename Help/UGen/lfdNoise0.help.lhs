    Sound.SC3.UGen.Help.viewSC3Help "LFDNoise0"
    Sound.SC3.UGen.DB.ugenSummary "LFDNoise0"

> import Sound.SC3 {- hsc3 -}

for fast x LFNoise frequently seems stuck, LFDNoise changes smoothly

> g_01 = lfdNoise0 'a' AR (mouseX KR 0.1 1000 Exponential 0.2) * 0.1
>
> g_02 = lfNoise0 'a' AR (mouseX KR 0.1 1000 Exponential 0.2) * 0.1

silent for 2 secs before going up in freq

> g_03 = lfdNoise0 'a' AR (xLine KR 0.5 10000 3 RemoveSynth)
>
> g_04 = lfNoise0 'a' AR (xLine KR 0.5 10000 3 RemoveSynth)

LFNoise quantizes time steps at high freqs, LFDNoise does not:

> g_05 = lfdNoise0 'a' AR (xLine KR 1000 20000 10 RemoveSynth)
>
> g_06 = lfNoise0 'a' AR (xLine KR 1000 20000 10 RemoveSynth)

Drawings

    import Sound.SC3.Plot {- hsc3-plot -}
    plot_ugen1 0.1 (lfdNoise0 'γ' AR 1000)
    plot_ugen1 0.1 (lfdNoise1 'γ' AR 1000)
    plot_ugen1 0.1 (lfdNoise3 'γ' AR 1000)

![](sw/hsc3/Help/SVG/lfdNoise0.0.svg)
![](sw/hsc3/Help/SVG/lfdNoise0.1.svg)
![](sw/hsc3/Help/SVG/lfdNoise0.2.svg)
