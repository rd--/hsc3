> import Sound.SC3

> g_01 =
>     let a = lfPulse AR 800 0 0.5 * 0.1 + 0.5
>     in mce [a,leakDC a 0.995]
