> Sound.SC3.UGen.Help.viewSC3Help "LFTri"
> Sound.SC3.UGen.DB.ugenSummary "LFTri"

> import Sound.SC3

> audition (out 0 (lfTri AR 500 1 * 0.1))

Used as both Oscillator and LFO.
> audition (out 0 (lfTri AR (lfTri KR 4 0 * 400 + 400) 0 * 0.1))

Multiple phases
> let f = lfTri KR 0.4 (mce [0..3]) * 200 + 400
> in audition (out 0 (mix (lfTri AR f 0 * 0.1)))
