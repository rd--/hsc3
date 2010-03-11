default (jmcc)

> import Sound.SC3.Monadic
> import Sound.OpenSoundControl
> import System.Random

> main =
>   let { def = do { r0 <- rand (-0.4) 0.0
>                  ; r1 <- rand 0.0 0.4
>                  ; r2 <- rand 4000 5000
>                  ; r3 <- rand 2500 3200
>                  ; let { f = control KR "freq" 440
>                        ; a = control KR "amp" 0.1
>                        ; p = control KR "pan" 0
>                        ; g = control KR "gate" 1
>                        ; e = linen g 0.01 (a * 0.7) 0.3 RemoveSynth
>                        ; f3 = mce [f, f + r0, f + r1]
>                        ; l = xLine KR r2 r3 1 DoNothing
>                        ; z = lpf (mix (varSaw AR f3 0 0.3)) l * e }
>                    in return (pan2 z p 1) }
>       ; rrand lr = getStdRandom (randomR lr)
>       ; tone fd nid = do { pan <- rrand (-1, 1)
>                          ; amp <- rrand (0.1, 0.3)
>                          ; indx <- rrand (0, 7)
>                          ; let note = [60, 62, 64, 65, 67, 69, 71, 72] !! indx
>                            in send fd (s_new "default" nid AddToTail 1
>                                        [("freq", midiCPS note)
>                                        ,("pan", pan)
>                                        ,("amp", amp)])
>                          ; pauseThread 0.075
>                          ; send fd (n_set nid [("gate", 0)])
>                          ; pauseThread 0.075 } }
>   in withSC3 (\fd -> do { u <- def
>                         ; async fd (d_recv (synthdef "default" (out 0 u)))
>                         ; reset fd
>                         ; mapM_ (tone fd) [1024..1036] })
