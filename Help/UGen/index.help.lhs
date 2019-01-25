    > Sound.SC3.UGen.Help.viewSC3Help "Index"
    > Sound.SC3.UGen.DB.ugenSummary "Index"

> import Sound.SC3 {- hsc3 -}

Index buffer for frequency values

> g_01 =
>   let b = asLocalBuf 'α' [50,100,200,400,800,1600]
>       f = index b (range 0 6 (lfSaw KR 2 0))
>   in sinOsc AR (mce [f,f * 9]) 0 * 0.1

> g_02 =
>   let b = asLocalBuf 'α' [200, 300, 400, 500, 600, 800]
>       f = index b (mouseX KR 0 7 Linear 0.2)
>   in sinOsc AR f 0 * 0.1
