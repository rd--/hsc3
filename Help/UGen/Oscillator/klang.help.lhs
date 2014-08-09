> Sound.SC3.UGen.Help.viewSC3Help "Klang"
> Sound.SC3.UGen.DB.ugenSummary "Klang"

# SC2 had mul/add inputs.

> import Sound.SC3.ID

> let {f = [440,550..1100]
>     ;a = take 7 (cycle [0.05, 0.02])
>     ;p = replicate 7 0}
> in audition (out 0 (klang AR 1 0 (klangSpec f a p)))

play({Klang.ar(`[[800,1000,1200],[0.3,0.3,0.3],[pi,pi,pi]],1,0)*0.4})

> let s = klangSpec [800,1000,1200] [0.3,0.3,0.3] [pi,pi,pi]
> in audition (out 0 (klang AR 1 0 s * 0.4))

play({Klang.ar(`[[800,1000,1200],nil,nil],1,0)*0.25})

> let s = klangSpec [800,1000,1200] [1,1,1] [0,0,0]
> in audition (out 0 (klang AR 1 0 s * 0.25))

play({Klang.ar(`[Array.rand(12,600.0,1000.0),nil,nil],1,0)*0.05})

> let {f = map (\z -> rand z 600 1000) ['a'..'l']
>     ;s = klangSpec f (replicate 12 1) (replicate 12 0)}
> in audition (out 0 (klang AR 1 0 s * 0.05))

