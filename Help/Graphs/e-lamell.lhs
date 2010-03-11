e-lamell (rd)

> import Control.Monad
> import Sound.OpenSoundControl
> import Sound.SC3.Monadic
> import System.Random

> main =
>   let { now = NTPi 1
>       ; rrand l r = getStdRandom (randomR (l, r))
>       ; exp_rrand a b = do { n <- rrand 0 1
>                            ; let r = b / a 
>                              in return ((r ** n) * a) }
>       ; choose l = return . (l !!) =<< rrand 0 (length l - 1)
>       ; sendSynth fd n u = async fd (d_recv (synthdef n u))
>       ; e_lamell = let { ctl s v = control kr s v
>                        ; f = ctl "f" 440
>                        ; n = ctl "n" 12
>                        ; d = ctl "d" 0.1
>                        ; l = ctl "l" 0
>                        ; a = ctl "a" 1 }
>                    in do { t <- tChoose 1 (mce2 1 32)
>                          ; let { h = line ar n t d DoNothing
>                                ; s = blip ar f h
>                                ; e_d = envPerc 0.005 d
>                                ; e = envGen ar 1 a 0 1 RemoveSynth e_d }
>                            in return (out 0 (pan2 s l e)) }
>       ; r_note o p = do { oe <- choose o
>                         ; pe <- choose p
>                         ; return (oe * 12 + pe) }
>       ; l_sel = r_note [2, 3] [0]
>       ; h_sel = r_note [2, 3, 4] [0]
>       ; mk_s_new f n d a l = s_new "blip" (-1) AddToTail 1
>                                    [("f", f)
>                                    ,("n", n)
>                                    ,("d", d)
>                                    ,("a", a)
>                                    ,("l", l)]
>       ; pattern fd = do { p <- do { f <- liftM midiCPS l_sel
>                                   ; n <- rrand 2 36
>                                   ; d <- exp_rrand 0.01 0.4
>                                   ; a <- rrand 0 0.75
>                                   ; l <- rrand (-1) 1
>                                   ; return (mk_s_new f n d a l) }
>                         ; q <- do { f <- liftM midiCPS h_sel
>                                   ; n <- rrand 2 36
>                                   ; d <- exp_rrand 0.01 0.4
>                                   ; a <- choose [0, 0.25, 0.5, 1]
>                                   ; l <- rrand (-1) 1
>                                   ; return (mk_s_new f n d a l) }
>                         ; send fd (Bundle now [p, q]) 
>                         ; pauseThread 0.1 } }
>   in withSC3 (\fd -> do { sendSynth fd "blip" =<< e_lamell
>                         ; replicateM_ 64 (pattern fd) })
