> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

> g_01 =
>   let i = impulse KR 10 0
>       f = stepper i 0 4 16 (-3) 4 * 100
>   in sinOsc AR f 0 * 0.1

Using Stepper and BufRd for sequencing

> compose :: [t -> t] -> t -> t
> compose = foldl (flip (.)) id

> rvb :: ID z => z -> UGen -> UGen
> rvb z0 s =
>   let f z1 i =
>         let dly = mce [rand ((z0,z1),'α') 0 0.5
>                       ,rand ((z0,z1),'β') 0 0.5]
>         in allpassN i 0.05 dly (rand z1 1.5 2)
>   in compose (map f (id_seq 5 'γ')) s

> stpr :: UGen
> stpr =
>   let rate = mouseX KR 1.75 2.25 Exponential 0.1
>       clock = impulse KR rate 0
>       envl = decay2 clock 0.002 2.5
>       indx = stepper clock 0 0 15 1 0
>       freq = bufRdN 1 KR 10 indx Loop
>       ffreq = lag2 freq 0.1 + mce [0,0.3]
>       lfo = sinOsc KR 0.2 (mce [0,pi/2]) * 0.0024 + 0.0025
>       top = mix (lfPulse AR (freq * mce [1,1.5,2]) 0 0.3)
>       chn = [\s -> rlpf s ffreq 0.3 * envl
>             ,\s -> rlpf s ffreq 0.3 * envl
>             ,\s -> s * 0.5
>             ,\s -> combL s 1 (0.66 / rate) 2 * 0.8 + s
>             ,\s -> s + (rvb 'γ' s * 0.3)
>             ,\s -> leakDC s 0.1
>             ,\s -> delayL s 0.1 lfo + s
>             ,\s -> onePole s 0.9]
>   in compose chn top

> stpr_init :: DuplexOSC m => m ()
> stpr_init = do
>   let n = [97.999,195.998,523.251,466.164,195.998
>           ,233.082,87.307,391.995,87.307,261.626
>           ,195.998,77.782,233.082,195.998,97.999
>           ,155.563]
>   _ <- async (b_alloc 10 128 1)
>   sendMessage (b_setn 10 [(0,n)])

    > withSC3 stpr_init
    > audition stpr
