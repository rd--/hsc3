> Sound.SC3.UGen.Help.viewSC3Help "BufWr"
> Sound.SC3.UGen.DB.ugenSummary "BufWr"

> import Sound.SC3 {- hsc3 -}

allocate a buffer (id = 0) for writing into

> withSC3 (send (b_alloc 0 (44100 * 2) 1))

write into the buffer (id = 0) with a bufWr

> let wr_gr =
>     let {rt = control KR "wr-rate" 1
>         ;o = sinOsc AR (lfNoise1 'α' KR 2 * 300 + 400) 0 * 0.1
>         ;w = bufWr 0 (phasor AR 0 (bufRateScale KR 0 * rt) 0 (bufFrames KR 0) 0) Loop o}
>     in out 0 (mrg2 (silent 1) w)

read it with a BufRd

> let rd_gr =
>     let {rt = control KR "rd-rate" 1
>         ;r = bufRdL 1 AR 0 (phasor AR 0 (bufRateScale KR 0 * rt) 0 (bufFrames KR 0) 0) Loop}
>     in out 0 r

> audition wr_gr
> audition rd_gr

set read & write rates independently

> withSC3 (send (n_set1 1 "wr-rate" 0.25))
> withSC3 (send (n_set1 1 "rd-rate" 13.0))
