> Sound.Sc3.UGen.Help.viewSc3Help "DecodeB2"
> Sound.Sc3.UGen.DB.ugenSummary "DecodeB2"

> import Sound.Sc3

fails..., but so does sclang...

> let {p = pinkNoiseId 'α' ar
>     ;mx = mouseX kr (-1) 1 Linear 0.2
>     ;my = mouseY kr 0 0.1 Linear 0.2
>     ;[w,x,y] = mceChannels (panB2 p mx my)
>     ;o = decodeB2 2 w x y 0.5}
> in audition (out 0 o)
