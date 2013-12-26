> Sound.SC3.UGen.Help.viewSC3Help "Splay"
splay inArray spread=1 level=1 center=0 levelComp=true

splay is a composite UGen.

> import Sound.SC3.ID

mouse control

> let {i = 6
>     ;r = map (\e -> rand e 10 20) (take i ['a'..])
>     ;n = lfNoise2 'a' KR (mce r)
>     ;x = mouseX KR (-1) 1 Linear 0.1
>     ;y = mouseY KR 1 0 Linear 0.1
>     ;ci = constant . fromIntegral
>     ;f = mce [1 .. ci i] + 3 * 100
>     ;o = sinOsc AR (n * 200 + f) 0}
> in audition (out 0 (splay o y 0.2 x True))

n_set control

> let {i = 10
>     ;s = control KR "spread" 1
>     ;l = control KR "level" 0.2
>     ;c = control KR "center" 0
>     ;r = map (\e -> rand e 10 20) (take i ['a'..])
>     ;ci = constant . fromIntegral
>     ;f = mce [1 .. ci i] + 3 * 100
>     ;n = lfNoise2 'a' KR (mce r) * 200 + f}
> in audition (out 0 (splay (sinOsc AR n 0) s l c True))

full stereo

> withSC3 (send (n_set (-1) [("spread",1),("center",0)]))

less wide

> withSC3 (send (n_set (-1) [("spread",0.5),("center",0)]))

mono center

> withSC3 (send (n_set (-1) [("spread",0),("center",0)]))

from center to right

> withSC3 (send (n_set (-1) [("spread",0.5),("center",0.5)]))

all left

> withSC3 (send (n_set (-1) [("spread",0),("center",-1)]))
