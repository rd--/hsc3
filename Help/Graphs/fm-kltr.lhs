fm-kltr (rd)

> import Sound.OpenSoundControl
> import Sound.SC3.Monadic
> import System.Random

> main =
>   let { rrand l r = getStdRandom (randomR (l, r))
>       ; gr =
>             do { r1 <- rand 0.975 1.025
>                ; r2 <- rand 0.5 1.5
>                ; r3 <- rand 0.975 1.025
>                ; r4 <- rand 0.75 1.25
>                ; let { o = control ir "out" 0
>                      ; t = control kr "trig" 0 {- unused -}
>                      ; a = control kr "amp" 0.1
>                      ; d = control kr "dur" 0.1
>                      ; f = control kr "freq" 400.0
>                      ; i = control kr "index" 40.0
>                      ; p = control kr "pan" 0.0
>                      ; f2 = control kr "freq2" 600
>                      ; ep = envPerc 0.01 d
>                      ; e = envGen ar 1 a 0 1 RemoveSynth ep
>                      ; so = sinOsc ar (xLine kr f (f * r1) d DoNothing) 0
>                      ; xl = xLine kr f2 (f2 * r3) d DoNothing
>                      ; m = so * line kr i (f * r2) d DoNothing + xl
>                      ; l = line kr p (p * r4) d DoNothing }
>                  in return (out o (pan2 (sinOsc ar m 0) l e)) }
>       ; fm fd f ff a d i =
>           do { r1 <- rrand (-1) 1
>              ; r2 <- rrand (-1) 1
>              ; send fd (s_new "fm" (-1) AddToTail 1
>                               [("freq", midiCPS f)
>                               ,("freq2", midiCPS ff + r1)
>                               ,("amp", a)
>                               ,("dur", d)
>                               ,("index", i)
>                               ,("pan", r2)]) }
>       ; nd fd =
>           do { ff <- rrand 48 96
>              ; a <- rrand 0.1 0.4
>              ; d <- rrand 1.2 7.2
>              ; i <- rrand 240 1480
>              ; t <- rrand 0.15 1.25
>              ; fm fd 53 ff a d i
>              ; pauseThread t } }
>   in withSC3 (\fd -> do { u <- gr
>                         ; async fd (d_recv (synthdef "fm" u))
>                         ; sequence (replicate 32 (nd fd)) })
