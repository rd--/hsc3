    Sound.SC3.UGen.Help.viewSC3Help "PV_BufRd"
    Sound.SC3.UGen.DB.ugenSummary "PV_BufRd"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

allocate analysis buffer and load soundfile

> n_01 = "/home/rohan/opt/src/supercollider/sounds/a11wlk01.wav"

> m_01 =
>   let f = 1024 {- frame size -}
>       h = 0.25 {- hop size -}
>       p_dur = 4.2832879818594 {- duration (in seconds) of n_01 -}
>       b_size = pv_calcPVRecSize p_dur f h 48000
>   in [b_alloc 0 b_size 1,b_allocRead 1 n_01 0 0]

    withSC3 (mapM_ async m_01)

do the analysis and store to buffer.  the window type and overlaps are
important for resynthesis parameters

> g_01 =
>   let rec_buf = 0
>       au_buf = 1
>       l_buf = localBuf 'α' 1024 1;
>       rt = bufRateScale KR au_buf
>       i = playBuf 1 AR au_buf rt 1 0 NoLoop RemoveSynth
>       c0 = fft l_buf i 0.25 1 1 0
>       c1 = pv_RecordBuf c0 rec_buf 0 1 0 0.25 1
>   in mrg2 (dc AR 0) c1

play analysis back

> g_02 =
>   let rec_buf = 0
>       l_buf = localBuf 'α' 1024 1
>       x = mouseX KR 0 1 Linear 0.2
>       c0 = pv_BufRd l_buf rec_buf x
>   in ifft c0 1 0
