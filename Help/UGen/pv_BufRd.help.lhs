> import Sound.SC3

allocate anazlysis buffer and load soundfile

> let {p = "/home/rohan/opt/share/SuperCollider/sounds/a11wlk01.wav"
>     ;f = 1024 {- frame size -}
>     ;h = 0.25 {- hop size -}
>     ;p_dur = 4.2832879818594 {- duration (in seconds) of p -}
>     ;b_size = pv_calcPVRecSize p_dur f h 48000}
> in withSC3 (do {_ <- async (b_alloc 0 b_size 1)
>                ;async (b_allocRead 1 p 0 0)})

do the analysis and store to buffer.  the window type and overlaps are
important for resynthesis parameters

> let {rec_buf = 0
>     ;au_buf = 1
>     ;l_buf = localBuf 'α' 1024 1;
>     ;rt = bufRateScale KR au_buf
>     ;i = playBuf 1 AR au_buf rt 1 0 NoLoop RemoveSynth
>     ;c0 = fft l_buf i 0.25 1 1 0
>     ;c1 = pv_RecordBuf c0 rec_buf 0 1 0 0.25 1}
> in audition (mrg2 (out 0 (dc AR 0)) c1)

play analysis back

> let {rec_buf = 0
>     ;l_buf = localBuf 'α' 1024 1
>     ;x = mouseX KR 0 1 Linear 0.2
>     ;c0 = pv_BufRd l_buf rec_buf x
>     ;s = ifft c0 1 0}
> in audition (out 0 s)
