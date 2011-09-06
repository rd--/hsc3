> Sound.SC3.UGen.Help.viewSC3Help "Dbufrd"
> Sound.SC3.UGen.DB.ugenSummary "Dbufrd"

> import Sound.SC3.ID
> import System.Random

setup pattern at buffer 10
> let n = randomRs (200.0,500.0) (mkStdGen 0)
> in withSC3 (\fd -> async fd (b_alloc_setn1 10 0 (take 24 n)))

pattern as frequency input
> let {s = dseq 'a' 3 (mce [0,3,5,0,3,7,0,5,9])
>     ;b = dbrown 'a' 5 0 23 1
>     ;p = dseq 'a' dinf (mce [s,b])
>     ;t = dust 'a' KR 10
>     ;r = dbufrd 'a' 10 p Loop}
> in audition (out 0 (sinOsc AR (demand t 0 r) 0 * 0.1))

setup time pattern
> let {i = randomRs (0,2) (mkStdGen 0)
>     ;n = map ([1,0.5,0.25] !!) i}
> in withSC3 (\fd -> async fd (b_alloc_setn1 11 0 (take 24 n)))

requires buffers 10 and 11 as allocated above
> let {s = dseq 'a' 3 (mce [0,3,5,0,3,7,0,5,9])
>     ;b = dbrown 'a' 5 0 23 1
>     ;p = dseq 'a' dinf (mce [s,b])
>     ;j = dseries 'a' dinf 0 1
>     ;d = dbufrd 'a' 11 j Loop
>     ;l = dbufrd 'a' 10 p Loop
>     ;f = duty KR (d * 0.5) 0 DoNothing l}
> in audition (out 0 (sinOsc AR f 0 * 0.1))

free buffers
> withSC3 (\fd -> do {async fd (b_free 10)
>                    ;async fd (b_free 11)})
