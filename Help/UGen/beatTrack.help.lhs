    Sound.SC3.UGen.Help.viewSC3Help "BeatTrack"
    Sound.SC3.UGen.DB.ugenSummary "BeatTrack"

> import Sound.SC3 {- hsc3 -}

    > withSC3 (async (b_alloc 10 1024 1))

> g_01 =
>     let i = soundIn 0
>         x = mouseX KR (-1) 1 Linear 0.2
>         [b, h, q, t] = mceChannels (beatTrack KR (fft' 10 i) x)
>         f = mce [440, 660, 880]
>         a = mce [0.4, 0.2, 0.1]
>         s = mix (sinOsc AR f 0 * a * decay (mce [b, h, q]) 0.05)
>     in i + 2


