    Sound.SC3.UGen.Help.viewSC3Help "LFDNoise3"
    Sound.SC3.UGen.DB.ugenSummary "LFDNoise3"

See lfdNoise0

Drawings

> import Sound.SC3 {- hsc3 -}

> g_01 = lfdNoise3 'α' AR (xLine AR 1000 100 0.1 DoNothing)

    > Sound.SC3.Plot.plot_ugen1 0.1 g_01

![](sw/hsc3/Help/SVG/lfdNoise3.0.svg)

