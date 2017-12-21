    Sound.SC3.UGen.Help.viewSC3Help "PV_MagGate"
    Sound.SC3.UGen.DB.ugenSummary "PV_MagGate"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> f_01 (lhs,rhs) =
>   let i = soundIn 0
>       c = fft' (localBuf 'α' 2048 1) i
>       x = mouseX KR lhs rhs Linear 0.2
>       y = mouseY KR 0 1 Linear 0.2
>       h = pv_MagGate c x y
>   in ifft' h * 0.5

> g_01 = f_01 (0,100)

> g_02 = f_01 (-50,0)
