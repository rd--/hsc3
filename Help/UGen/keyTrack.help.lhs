    > Sound.SC3.UGen.Help.viewSC3Help "KeyTrack"
    > Sound.SC3.UGen.DB.ugenSummary "KeyTrack"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

> g_01 =
>   let s = soundIn (mce2 0 1)
>       t = fft' (localBuf 'α' 1 4096) (mix s)
>       k = keyTrack KR t 2.0 0.5
>       p = poll (impulse KR 1 0) k 0 (label "key")
>   in mrg2 s p
