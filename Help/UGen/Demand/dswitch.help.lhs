> Sound.SC3.UGen.Help.viewSC3Help "Dswitch"
> Sound.SC3.UGen.DB.ugenSummary "Dswitch"

> import Sound.SC3

> do {a0 <- dwhiteM 2 3 4
>    ;a1 <- dwhiteM 2 0 1
>    ;a2 <- dseqM 2 (mce [1,1,1,0])
>    ;i <- dseqM 2 (mce [0,1,2,1,0])
>    ;d <- dswitchM i (mce [a0,a1,a2])
>    ;let {t = impulse KR 4 0
>         ;f = demand t 0 d * 300 + 400
>         ;o = sinOsc AR f 0 * 0.1}
>      in audition (out 0 o)}

compare with dswitch1

> do {a0 <- dwhiteM 2 3 4
>    ;a1 <- dwhiteM 2 0 1
>    ;a2 <- dseqM 2 (mce [1,1,1,0])
>    ;i <- dseqM 2 (mce [0,1,2,1,0])
>    ;d <- dswitch1M i (mce [a0,a1,a2])
>    ;let {t = impulse KR 4 0
>         ;f = demand t 0 d * 300 + 400
>         ;o = sinOsc AR f 0 * 0.1}
>      in audition (out 0 o)}

