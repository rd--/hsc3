> Sound.SC3.UGen.Help.viewSC3Help "DecodeB2"
> Sound.SC3.UGen.DB.ugenSummary "DecodeB2"

> import Sound.SC3

fails..., but so does sclang...

> let {p = pinkNoise 'α' AR
>     ;mx = mouseX KR (-1) 1 Linear 0.2
>     ;my = mouseY KR 0 0.1 Linear 0.2
>     ;[w,x,y] = mceChannels (panB2 p mx my)
>     ;o = decodeB2 2 w x y 0.5}
> in audition (out 0 o)
