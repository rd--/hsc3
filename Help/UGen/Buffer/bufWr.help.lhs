> Sound.SC3.UGen.Help.viewSC3Help "BufWr"
> Sound.SC3.UGen.DB.ugenSummary "BufWr"

> import Sound.SC3 {- hsc3 -}

allocate a buffer (id = 0) for writing into

> withSC3 (send (b_alloc 0 (44100 * 2) 1))

write into the buffer (id = 0) with a bufWr

> let {rt = control KR "wr-rate" 1
>     ;o = sinOsc AR (lfNoise1 'a' KR 2 * 300 + 400) 0 * 0.1
>     ;w = bufWr 0 (phasor AR 0 (bufRateScale KR 0 * rt) 0 (bufFrames KR 0) 0) Loop o}
> in audition (out 0 (mce2 (silent 2) w))

read it with a BufRd

> let {rt = control KR "rd-rate" 1
>     ;r = bufRdL 1 AR 0 (phasor AR 0 (bufRateScale KR 0 * rt) 0 (bufFrames KR 0) 0) Loop}
> in audition (out 0 r)

set read & write rates independently

> withSC3 (send (n_set1 0 "wr-rate" 2))
> withSC3 (send (n_set1 0 "rd-rate" 1))
