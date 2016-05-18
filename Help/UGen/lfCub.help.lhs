    > Sound.SC3.UGen.Help.viewSC3Help "LFCub"
    > Sound.SC3.UGen.DB.ugenSummary "LFCub"

> import Sound.SC3

> g_01 = lfCub AR (lfCub KR (lfCub KR 0.2 0 * 8 + 10) 0 * 400 + 800) 0 * 0.1
> g_02 = lfCub AR (lfCub KR 0.2 0 * 400 + 800) 0 * 0.1
> g_03 = lfCub AR 800 0 * 0.1
> g_04 = lfCub AR (xLine KR 100 8000 30 DoNothing) 0 * 0.1

Compare w/ lfPar

> g_05 = lfPar AR (lfPar KR (lfPar KR 0.2 0 * 8 + 10) 0 * 400 + 800) 0 * 0.1
> g_06 = lfPar AR (lfPar KR 0.2 0 * 400 + 800) 0 * 0.1
> g_07 = lfPar AR 800 0 * 0.1
> g_08 = lfPar AR (xLine KR 100 8000 30 DoNothing) 0 * 0.1

Compare w/ sinOsc

> g_09 = sinOsc AR (sinOsc KR (sinOsc KR 0.2 0 * 8 + 10) 0 * 400 + 800) 0 * 0.1
> g_11 = sinOsc AR (sinOsc KR 0.2 0 * 400 + 800) 0 * 0.1
> g_12 = sinOsc AR 800 0 * 0.1
> g_13 = sinOsc AR (xLine KR 100 8000 30 DoNothing) 0 * 0.1

Compare w/ lfTri

> g_14 = lfTri AR (lfTri KR (lfTri KR 0.2 0 * 8 + 10) 0 * 400 + 800) 0 * 0.1
> g_15 = lfTri AR (lfTri KR 0.2 0 * 400 + 800) 0 * 0.1
> g_16 = lfTri AR 800 0 * 0.1
> g_17 = lfTri AR (xLine KR 100 8000 30 DoNothing) 0 * 0.1

Drawings

    > import Sound.SC3.Plot {- hsc3-plot -}
    > plot_ugen 0.1 (mce2 (sinOsc AR 20 0) (lfCub AR 20 0))
    > plot_ugen 0.1 (mce2 (sinOsc AR 20 0) (lfPar AR 20 pi))

![](sw/hsc3/Help/SVG/lfCub.0.svg)
![](sw/hsc3/Help/SVG/lfCub.1.svg)
