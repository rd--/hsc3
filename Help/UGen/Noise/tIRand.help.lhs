> Sound.SC3.UGen.Help.viewSC3Help "TIRand"
> Sound.SC3.UGen.DB.ugenSummary "TIRand"

> import Sound.SC3
> import qualified Sound.SC3.Monadic as M

> do {l <- M.tIRand (-1) 1 =<< M.dust KR 10
>    ;n <- M.pinkNoise AR
>    ;audition (out 0 (pan2 (n * 0.1) l 1))}

> do {n <- M.tIRand 4 12 =<< M.dust KR 10
>    ;let f = n * 150 + (mce [0,1])
>     in audition (out 0 (sinOsc AR f 0 * 0.1))}
