trmlo (rd)

> import System.IO.Unsafe

> let { u_iRand p q = unsafePerformIO (iRand p q)
>     ; u_rand p q = unsafePerformIO (rand p q)
>     ; mWrp i l r = linLin i (-1) 1 (midiCPS l) (midiCPS r)
>     ; mWrp1 i m = mWrp i m (m + 1)
>     ; mWrpN i m n = mWrp i m (m + n)
>     ; o1 = let { f = 5
>                ; d = 3
>                ; s = envSine d 0.1
>                ; e = envGen kr 1 1 0 1 DoNothing s
>                ; n = 65
>                ; m = sinOsc kr f 0 }
>            in pan2 (sinOsc ar (mWrp1 m n) 0) m e
>     ; o2 = let { f = u_iRand 5 9
>                ; d = u_iRand 5 9
>                ; s = envSine d (u_rand 0.1 0.2)
>                ; e = envGen kr 1 1 0 1 DoNothing s
>                ; n = u_iRand 69 72
>                ; m = sinOsc kr f 0 }
>            in pan2 (sinOsc ar (mWrp1 m n) 0) m e
>     ; o3 = let { f = u_iRand 5 9
>                ; d = u_iRand 9 12
>                ; s = envSine d (u_rand 0.1 0.2)
>                ; e = envGen kr 1 1 0 1 DoNothing s
>                ; n = u_iRand 69 72
>                ; m = sinOsc kr f 0
>                ; l = line kr 0 (u_iRand 1 5) d DoNothing }
>            in pan2 (blip ar (mWrp1 m (n + l)) (linLin m (-1) 1 1 2)) m e
>     ; o4 = let { f = u_iRand 5 18
>                ; d = u_iRand 12 15
>                ; s = envSine d (u_rand 0.1 0.2)
>                ; e = envGen kr 1 5e-2 0 1 DoNothing s
>                ; n = u_iRand 69 72
>                ; m = sinOsc kr f 0
>                ; l = line kr 0 (u_iRand 1 5) d RemoveSynth
>                ; fr = mWrpN m (n + l) (u_iRand 1 5) }
>            in pan2 (blip ar fr (linLin m (-1) 1 1 (u_iRand 2 24))) m e }
> in audition (out 0 (o1 + o2 + o3 + o4))
