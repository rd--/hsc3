wslib: external/composite

> import Sound.SC3

Load sound file to buffer zero (single channel file required for examples)

> let {fn' = "/home/rohan/data/audio/pf-c5.aif"
>     ;fn = "/home/rohan/opt/share/SuperCollider/sounds/a11wlk01.wav"}
> in withSC3 (async (b_allocRead 0 fn 0 0))

control-rate trigger and start-position inputs

> let {b = 0
>     ;r = bufRateScale KR b
>     ;tr = impulse KR 2 0
>     ;wn = whiteNoise 'α' KR
>     ;sp = linLin wn (-1) 1 0 (bufFrames KR b - (0.5 * 44100))
>     ;o = playBufCF 1 b r tr sp NoLoop 0.1 2
>     ;o' = playBuf 1 AR b r tr sp NoLoop DoNothing}
> in audition (out 0 (mce2 o o'))

demand ugens inputs

> let {b = 0
>     ;r = drand 'α' dinf (mce [0.95,1,1.05])
>     ;tr = dwhite 'β' dinf 0.1 0.3
>     ;sp = dbrown 'γ' dinf 0 0.95 0.1 * bufFrames KR b
>     ;o = playBufCF 1 b r tr sp NoLoop 2 5}
> in audition (out 0 o)

for drawings...

> import Sound.SC3.UGen.Dot {- hsc3-dot -}
