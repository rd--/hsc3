    Sound.SC3.UGen.Help.viewSC3Help "PV_Freeze"
    Sound.SC3.UGen.DB.ugenSummary "PV_Freeze"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> g_01 =
>   let f = fft' (localBuf 'α' 2048 1) (soundIn 0)
>       x = mouseX KR 0 1 Linear 0.1
>       h = pv_Freeze f (x >* 0.5)
>   in ifft' h * 0.5
