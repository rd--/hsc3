> import Sound.SC3 {- hsc 3 -}

Load sound file to buffer zero (required for examples)

> fn_01 = "/home/rohan/rd/j/2019-04-21/FAIRLIGHT/IIX/REEDS/clarmdhi.snd"

> m_01 = b_allocRead 0 fn_01 0 0

    > withSC3 (async m_01)

> f_01 m =
>     let r = m * bufRateScale KR 0
>         p = phasor AR 0 r 0 (bufFrames KR 0) 0
>     in bufRdL 1 AR 0 p NoLoop

> g_01 = f_01 1.0 * 0.5

    > audition (sinOsc AR (midiCPS 69) 0 * 0.2)

Read buffer at ~ 3/4 reported sample rate.

> g_02 = f_01 (midiRatio (-5)) * 0.5

    > audition (sinOsc AR (midiCPS 64) 0 * 0.2)
