fm-kltr (rd)

> let { rrand l r = getStdRandom (randomR (l, r))
>     ; threadPause :: Double -> IO ()
>     ; threadPause t = when (t>0) (threadDelay (floor (t * 1e6)))
>     ; gr =
>           do { r1 <- rand 0.975 1.025
>              ; r2 <- rand 0.5 1.5
>              ; r3 <- rand 0.975 1.025
>              ; r4 <- rand 0.75 1.25
>              ; let { o = Control KR "out" 0
>                    ; t = Control KR "trig" 0
>                    ; a = Control KR "amp" 0.1
>                    ; d = Control KR "dur" 0.1
>                    ; f = Control KR "freq" 400.0
>                    ; i = Control KR "index" 40.0
>                    ; p = Control KR "pan" 0.0
>                    ; f2 = Control KR "freq2" 600
>                    ; ep = envPerc 0.01 d
>                    ; e = envGen AR 1 a 0 1 RemoveSynth ep
>                    ; so = sinOsc AR (xLine KR f (f * r1) d DoNothing) 0
>                    ; xl = xLine KR f2 (f2 * r3) d DoNothing
>                    ; m = so * line KR i (f * r2) d DoNothing + xl
>                    ; l = line KR p (p * r4) d DoNothing }
>                in return (out o (pan2 (sinOsc AR m 0) l e)) }
>     ; fm fd f ff a d i =
>         do { r1 <- rrand (-1) 1
>            ; r2 <- rrand (-1) 1
>            ; send fd (s_new "fm" (-1) AddToTail 1
>                             [("freq", midiCPS f)
>                             ,("freq2", midiCPS ff + r1)
>                             ,("amp", a)
>                             ,("dur", d)
>                             ,("index", i)
>                             ,("pan", r2)]) }
>     ; nd fd =
>         do { ff <- rrand 48 96
>            ; a <- rrand 0.1 0.4
>            ; d <- rrand 1.2 7.2
>            ; i <- rrand 240 1480
>            ; t <- rrand 0.15 1.25
>            ; fm fd 53 ff a d i
>            ; threadPause t } }
> in withSC3 (\fd -> do { u <- gr
>                       ; async fd (d_recv (synthdef "fm" u))
>                       ; replicateM 32 (nd fd) })
