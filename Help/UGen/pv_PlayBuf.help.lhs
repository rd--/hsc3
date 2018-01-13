    Sound.SC3.UGen.Help.viewSC3Help "PV_PlayBuf"
    Sound.SC3.UGen.DB.ugenSummary "PV_PlayBuf"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

see pv_BufRd for code to allocate and fill analysis buffer (rec_buf)

> g_01 =
>   let rec_buf = 0
>       l_buf = localBuf 'α' 1024 1
>       x = mouseX KR (-1) 1 Linear 0.2
>       c = pv_PlayBuf l_buf rec_buf x 50 1
>   in ifft c 1 0

> g_02 =
>   let rec_buf = 0
>       l_buf = localBuf 'β' 1024 1
>       n = range (-1) 2 (lfNoise2 'γ' KR 0.2)
>       c = pv_PlayBuf l_buf rec_buf n 0 1
>   in ifft c 1 0
