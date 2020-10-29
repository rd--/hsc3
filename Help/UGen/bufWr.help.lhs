> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let b = localBuf 'α' 1 (48000 * 2)
>       o = sinOsc AR (lfNoise1 'β' KR 2 * 300 + 400) 0 * 0.1
>       ph z = phasor AR 0 (bufRateScale KR b * z) 0 (bufFrames KR b) 0
>       w = bufWr b (ph (mouseX KR 0.25 1 Linear 0.2)) Loop o
>       r = bufRdL 1 AR b (ph (mouseY KR 0.25 16 Linear 0.2)) Loop
>   in mrg2 r w

allocate a buffer (id = 0) for writing into

    > withSC3 (async (b_alloc 0 (44100 * 2) 1))

write into the buffer (id = 0) with a bufWr

> wr_gr =
>     let rt = control KR "wr-rate" 1
>         o = sinOsc AR (lfNoise1 'α' KR 2 * 300 + 400) 0 * 0.1
>         w = bufWr 0 (phasor AR 0 (bufRateScale KR 0 * rt) 0 (bufFrames KR 0) 0) Loop o
>     in mrg2 (silent 1) w

read it with a BufRd

> rd_gr =
>     let rt = control KR "rd-rate" 1
>     in bufRdL 1 AR 0 (phasor AR 0 (bufRateScale KR 0 * rt) 0 (bufFrames KR 0) 0) Loop

    > audition wr_gr
    > audition rd_gr

set read & write rates independently

    > import Sound.OSC {- hosc -}
    > withSC3 (sendMessage (n_set1 1 "wr-rate" 0.25))
    > withSC3 (sendMessage (n_set1 1 "rd-rate" 13.0))
