train (th.list at gmail.com)

> import Sound.SC3.Monadic

> main =
>   let { time = 24
>       ; steam n1 n2 = let { piston = lfSaw AR (xLine AR 1 7 time DoNothing) 0
>                           ; air = lpf (n1 * piston + n2 * piston) 5000 
>                           ; e = envGen AR 1 1 0 1 DoNothing (envSine time 9) }
>                       in bpf air 600 (1 + e)
>       ; whistle n3 = let { f = [800, 600, 1200, 990]
>                          ; s = klankSpec f [1, 1, 1, 1] [1, 1, 1, 1]
>                          ; t = [0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0]
>                          ; l = [2, 0, 0.2, 0, 0.2, 0, 0.8, 0, 4, 0, 3]
>                          ; d = env t l (repeat EnvLin) 0 0
>                          ; e = envGen AR 1 1 0 (time/10) DoNothing d }
>                      in klank (n3 * 0.004) 1 0 1 s * e
>       ; loc = let e = env [-0.8, 0.8] [time + 2] [EnvSin, EnvSin] 0 0
>               in envGen AR 1 1 0 1 RemoveSynth e }
>   in do { n1 <- whiteNoise AR
>         ; n2 <- pinkNoise AR
>         ; n3 <- whiteNoise AR
>         ; audition (out 0 (pan2 (steam n1 n2 + whistle n3) loc 1)) }

http://www.create.ucsb.edu/pipermail/sc-users/2007-August/035957.html