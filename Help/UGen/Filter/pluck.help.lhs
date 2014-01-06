> Sound.SC3.UGen.Help.viewSC3Help "Pluck"
> Sound.SC3.UGen.DB.ugenSummary "Pluck"

> import Sound.SC3.ID {- hsc3 -}

Excitation signal is white noise, triggered twice a second with
varying OnePole coef.

> let {n = whiteNoise 'α' AR
>     ;t = impulse KR 9 0
>     ;x = mouseX KR (-0.999) 0.999 Linear 0.1
>     ;y = mouseY KR 0.1 1 Linear 0.1
>     ;dl = 1 / 440}
> in audition (out 0 (pluck (n * 0.25) t dl (dl * y) 10 x))

> import Sound.SC3.UGen.Protect {- hsc3 -}

> let {n = 50
>     ;udup = uclone 'α'
>     ;f = udup n (rand 'β' 0.05 0.2)
>     ;p = udup n (rand 'γ' 0 1)
>     ;w = udup n (whiteNoise 'δ' AR)
>     ;fi = udup n (rand 'ε' 10 12)
>     ;coef = rand 'ζ' 0.01 0.2
>     ;l = udup n (rand 'η' (-1) 1)
>     ;x = mouseX KR 60 1000 Exponential 0.1
>     ;o = linLin (sinOsc KR f p) (-1) 1 x 3000
>     ;i = impulse KR fi 0
>     ;ks = pluck (w * 0.1) i 0.01 (1 / o) 2 coef}
> in audition (out 0 (leakDC (mix (pan2 ks l 1)) 0.995))
