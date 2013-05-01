> Sound.SC3.UGen.Help.viewSC3Help "Dswitch"
> Sound.SC3.UGen.DB.ugenSummary "Dswitch"
# inputReordering: [1,0]

> import Sound.SC3
> import qualified Sound.SC3.Monad as M

> do {a0 <- M.dwhite 2 3 4
>    ;a1 <- M.dwhite 2 0 1
>    ;a2 <- M.dseq 2 (mce [1,1,1,0])
>    ;i <- M.dseq 2 (mce [0,1,2,1,0])
>    ;d <- M.dswitch i (mce [a0,a1,a2])
>    ;let {t = impulse KR 4 0
>         ;f = demand t 0 d * 300 + 400
>         ;o = sinOsc AR f 0 * 0.1}
>      in audition (out 0 o)}

compare with dswitch1
> do {a0 <- M.dwhite 2 3 4
>    ;a1 <- M.dwhite 2 0 1
>    ;a2 <- M.dseq 2 (mce [1,1,1,0])
>    ;i <- M.dseq 2 (mce [0,1,2,1,0])
>    ;d <- M.dswitch1 i (mce [a0,a1,a2])
>    ;let {t = impulse KR 4 0
>         ;f = demand t 0 d * 300 + 400
>         ;o = sinOsc AR f 0 * 0.1}
>      in audition (out 0 o)}

