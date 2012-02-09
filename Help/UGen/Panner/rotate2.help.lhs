> Sound.SC3.UGen.Help.viewSC3Help "Rotate2"
> Sound.SC3.UGen.DB.ugenSummary "Rotate2"

> import Sound.SC3.ID

Rotation of stereo sound, via LFO.
> let {x = pinkNoise 'a' AR
>     ;y = lfTri AR 800 0 * lfPulse KR 3 0 0.3 * 0.2}
> in audition (out 0 (rotate2 x y (lfSaw KR 0.1 0)))

Rotation of stereo sound, via mouse.
> let {x = mix (lfSaw AR (mce [198..201]) 0 * 0.1)
>     ;y = sinOsc AR 900 0 * lfPulse KR 3 0 0.3 * 0.2
>     ;p = mouseX KR 0 2 Linear 0.2}
> in audition (out 0 (rotate2 x y p))
