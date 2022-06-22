> import Sound.Sc3 {- hsc3 -}
> import qualified Sound.Sc3.UGen.Bindings.DB.External as X {- hsc3 -}

> fft_sz = 2048::Int
> hop_sz = fft_sz `div` 2
> fn_0 = "/home/rohan/data/audio/pf-c5.snd"
> fn_1 = "/home/rohan/data/audio/material/tyndall/var/talking-fragments/0001.WAV"
> tpv' b i = X.tpv (fft b i 0.5 1 1 0) (constant fft_sz) (constant hop_sz)
> msg = [b_alloc 0 fft_sz 1,b_allocRead 1 fn_1 0 0]

    > withSc3 (mapM_ async msg)

> g_01 =
>     let i = playBuf 1 ar 1 (bufRateScale kr 1) 1 0 Loop DoNothing
>         x = mouseX kr 1 70 Linear 0.1
>         y = mouseY kr 0.25 3 Linear 0.1
>         o = tpv' 0 i 70 x y 4 0.2
>     in mce2 (i * 0.1) o

> g_02 =
>     let i = playBuf 1 ar 1 (bufRateScale kr 1) 1 0 Loop DoNothing
>         x = mouseX kr 0.1 100 Linear 0.1
>         y = mouseY kr (-20) 40 Linear 0.1
>         o = tpv' 0 i 50 50 1 x (dbAmp y)
>     in pan2 o 0 1
