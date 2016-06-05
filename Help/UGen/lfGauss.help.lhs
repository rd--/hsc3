    Sound.SC3.UGen.Help.viewSC3Help "LFGauss"
    Sound.SC3.UGen.DB.ugenSummary "LFGauss"

> import Sound.SC3 {- hsc3 -}

modulating duration

> g_01 =
>     let d = xLine KR 0.1 0.001 10 DoNothing
>     in lfGauss AR d 0.03 0 Loop DoNothing * 0.2

modulating width, freq 60 Hz

> g_02 =
>     let w = xLine KR 0.1 0.001 10 DoNothing
>     in lfGauss AR (1/60) w 0 Loop DoNothing * 0.2

modulating both: x position is frequency, y is width factor.
note the artefacts due to alisasing at high frequencies

> g_03 =
>     let d = mouseX KR (1/8000) 0.1 Exponential 0.2
>         w = mouseY KR 0.001 0.1 Exponential 0.2
>     in lfGauss AR d w 0 Loop DoNothing * 0.1

LFGauss as amplitude modulator

> g_04 =
>     let d = mouseX KR 1 0.001 Exponential 0.2
>         g = lfGauss AR d 0.1 0 Loop DoNothing
>         o = sinOsc AR 1000 0
>     in g * o * 0.1

modulate iphase

> g_05 =
>     let ph = mouseX KR (-1) 1 Linear 0.2
>         g = lfGauss AR 0.001 0.2 (mce2 0 ph) Loop DoNothing
>     in mix g * 0.2

for very small width we are "approaching" a dirac function

> g_06 =
>     let w = sampleDur * mouseX KR 10 3000 Exponential 0.2
>     in lfGauss AR 0.01 w 0 Loop DoNothing * 0.2

dur and width can be modulated at audio rate

> g_07 =
>     let x = mouseX KR 2 1000 Exponential 0.2
>         d = range 0.0006 0.01 (sinOsc AR x 0 * mce2 1 1.1)
>         w = range 0.01 0.3 (sinOsc AR (0.5 * (mce2 1 1.1)) 0)
>     in lfGauss AR d w 0 Loop DoNothing * 0.2

several frequencies and widths combined

> g_08 =
>     let x = mouseX KR 1 0.07 Exponential 0.2
>         y = mouseY KR 1 3 Linear 0.2
>         g = lfGauss AR x (y ** mce [-1,-2 .. -6]) 0 Loop DoNothing
>         o = sinOsc AR (200 * (1.3 ** mce [0..5])) 0
>     in mix (g * o) * 0.1

    > withSC3 (send (n_trace [-1]))

Drawing

    > import Sound.SC3.Plot {- hsc3-plot -}
    > let plot_f tm du ph = plot_ugen1 tm (lfGauss AR du 0.12 ph Loop DoNothing)
    > plot_f 0.1 0.1 0
    > plot_f 0.1 0.1 (-1) -- shifting left
    > plot_f 0.1 0.1 2 -- moving further away from the center
    > plot_f 0.3 0.065 0 -- several grains

![](sw/hsc3/Help/SVG/lfGauss.0.svg)
![](sw/hsc3/Help/SVG/lfGauss.1.svg)
![](sw/hsc3/Help/SVG/lfGauss.2.svg)
![](sw/hsc3/Help/SVG/lfGauss.3.svg)

gabor grain (see also 'gabor_grain_ugen_graph')

> gabor =
>     let b = control IR "out" 0
>         f = control IR "freq" 440
>         s = control IR "sustain" 1
>         p = control IR "pan" 0
>         a = control IR "amp" 0.1
>         w = control IR "width" 0.25
>         e = lfGauss AR s w 0 NoLoop RemoveSynth
>         o = fSinOsc AR f (pi / 2) * e
>         u = offsetOut b (pan2 o p a)
>     in synthdef "gabor" u

    > import Sound.SC3.Lang.Pattern {- hsc3-lang -}

granular synthesis, modulating duration, width and pan

    > paudition (pbind [(K_instr,psynth gabor)
    >                  ,(K_freq,1000)
    >                  ,(K_legato,2)
    >                  ,(K_dur,pbrown 'α' 0.005 0.025 0.001 inf)
    >                  ,(K_param "width",pbrown 'β' 0.05 0.25 0.005 inf)
    >                  ,(K_param "pan",pbrown 'γ' (-1) 1 0.05 inf)])

granular synthesis, modulating width only

    > paudition (pbind [(K_instr,psynth gabor)
    >                  ,(K_freq,1000)
    >                  ,(K_dur,0.01)
    >                  ,(K_param "width",pgeom 0.25 0.995 1250)
    >                  ,(K_legato,2)])
