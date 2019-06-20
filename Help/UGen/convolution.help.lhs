> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let k = pinkNoise 'α' AR * 0.1
>       i = soundIn 0
>   in convolution AR i k 2048

> g_02 =
>   let k = mix (lfSaw AR (mce [300,500,800,1000] * mouseX KR 1.0 2.0 Linear 0.2) 0 * 0.1)
>       i = soundIn 0
>   in convolution AR i k 1024 * 0.5
