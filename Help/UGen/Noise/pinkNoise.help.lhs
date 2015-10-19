> Sound.SC3.UGen.Help.viewSC3Help "PinkNoise"
> Sound.SC3.UGen.DB.ugenSummary "PinkNoise"

> import Sound.SC3

> audition . (out 0) . (* 0.05) =<< pinkNoiseM AR
> audition . (out 0) . (* 0.05) =<< whiteNoiseM AR
> audition . (out 0) . (* 0.05) =<< brownNoiseM AR

speaker balance

> let n = pinkNoise 'α' AR * 0.05 in audition (out 0 (mce2 n n))

> let {x = mouseX KR 0 1 Linear 0.2
>     ;x' = 1 - x
>     ;n = pinkNoise 'α' AR * 0.05}
> in audition (out 0 (mce2 (n * x') (n * x)))
