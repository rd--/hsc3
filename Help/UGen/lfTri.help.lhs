    Sound.SC3.UGen.Help.viewSC3Help "LFTri"
    Sound.SC3.UGen.DB.ugenSummary "LFTri"

see <http://thread.gmane.org/gmane.comp.audio.supercollider.user/84719>

> import Sound.SC3 {- hsc3 -}
>
> g_01 = lfTri AR 500 1 * 0.1

Used as both Oscillator and LFO.

> g_02 = lfTri AR (lfTri KR 4 0 * 400 + 400) 0 * 0.1

Multiple phases

> g_03 = let f = lfTri KR 0.4 (mce [0..3]) * 200 + 400
>        in mix (lfTri AR f 0 * 0.1)
>
> g_04 = let x = midiCPS (mouseX KR 20 72 Linear 0.2)
>            e = xLine KR 0.01 1 20 DoNothing
>            o1 = triAS 25 x * (1 - e)
>            o2 = lfTri AR x 0 * e
>        in mce2 o1 o2 * 0.1

Drawings

    > import Sound.SC3.Plot {- hsc3-plot -}
    > plot_ugen1 0.1 (lfTri AR 40 0)
    > plot_ugen1 0.1 (lfTri AR (xLine KR 1 800 0.1 DoNothing) 0)

![](sw/hsc3/Help/SVG/lfTri.0.svg)
![](sw/hsc3/Help/SVG/lfTri.1.svg)
