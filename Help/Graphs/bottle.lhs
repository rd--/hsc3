bottle (sc)

> import Control.Monad
> import Sound.SC3

> do { freq <- M.rand 220 880
>    ; wn <- M.whiteNoise ar
>    ; pn <- M.pinkNoise ar
>    ; let { chain n f = foldl (>=>) return (replicate n f)
>          ; perc = envPerc 0.1 0.6
>          ; ex = envGen kr 1 1 0 1 DoNothing perc * wn * 0.02
>          ; flute = ringz ex freq 0.3
>          ; r = resonz pn (5 + (freq / 2)) 0.1
>          ; breath = envGen kr 1 1 0 1 DoNothing perc * r
>          ; rapf i = do { x <- M.linRand 0.001 0.1 (-1)
>                        ; return (i + allpassN i 0.1 x 1.0 * 0.5) }
>          ; cls i = let { en = replicate 3 (EnvNum (-4))
>                        ; l = envLinen 0.01 3.0 1.0 1 en
>                        ; z = (breath + i) * envGen kr 1 1 0 1 RemoveSynth l }
>                    in mce2 z z }
>      in do { f <- (chain 2 rapf) flute
>            ; audition (out 0 (cls f)) } }
