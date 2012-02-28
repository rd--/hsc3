> Sound.SC3.UGen.Help.viewSC3Help "Stepper"
> Sound.SC3.UGen.DB.ugenSummary "Stepper"

> import Sound.SC3.ID

> let {i = impulse KR 10 0
>     ;f = stepper i 0 4 16 (-3) 4 * 100}
> in audition (out 0 (sinOsc AR f 0 * 0.1))

Using Stepper and BufRd for sequencing
> let {compose = foldl (flip (.)) id
>     ;rvb z s =
>         let f i = let dly = mce [rand (z `joinID` i `joinID` 'a') 0 0.5
>                                 ,rand (z `joinID` i `joinID` 'b') 0 0.5]
>                     in allpassN i 0.05 dly (rand i 1.5 2)
>         in compose (replicate 5 f) s
>     ;stpr = let {rate = mouseX KR 1.75 2.25 Exponential 0.1
>                 ;clock = impulse KR rate 0
>                 ;envl = decay2 clock 0.002 2.5
>                 ;indx = stepper clock 0 0 15 1 0
>                 ;freq = bufRdN 1 KR 10 indx Loop
>                 ;ffreq = lag2 freq 0.1 + mce [0,0.3]
>                 ;lfo = sinOsc KR 0.2 (mce [0,pi/2]) * 0.0024 + 0.0025
>                 ;top = mix (lfPulse AR (freq * mce [1,1.5,2]) 0 0.3)
>                 ;chn = [\s -> rlpf s ffreq 0.3 * envl
>                        ,\s -> rlpf s ffreq 0.3 * envl
>                        ,\s -> s * 0.5
>                        ,\s -> combL s 1 (0.66 / rate) 2 * 0.8 + s
>                        ,\s -> s + (rvb 'a' s * 0.3)
>                        ,\s -> leakDC s 0.1
>                        ,\s -> delayL s 0.1 lfo + s
>                        ,\s -> onePole s 0.9]}
>             in compose chn top
>     ;stprInit fd =
>      let n = [97.999,195.998,523.251,466.164,195.998
>              ,233.082,87.307,391.995,87.307,261.626
>              ,195.998,77.782,233.082,195.998,97.999
>              ,155.563]
>      in do {_ <- async fd (b_alloc 10 128 1)
>            ;send fd (b_setn 10 [(0,n)])}}
> in withSC3 (\fd -> do {stprInit fd
>                       ;audition (out 0 stpr)})
