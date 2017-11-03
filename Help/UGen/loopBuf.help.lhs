    Sound.SC3.UGen.Help.viewSC3Help "LoopBuf"
    Sound.SC3.UGen.DB.ugenSummary "LoopBuf"

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.HW.External.SC3_Plugins as E {- hsc3 -}

Read audio file into memory

> ld b = do
>   let fn = "/home/rohan/opt/src/supercollider/sounds/a11wlk01.wav"
>   withSC3 (async (b_allocRead b fn 0 0))

Simple sampler instrument

> lb0 =
>     let ou = control KR "out" 0
>         bf = control KR "bufnum" 0
>         rt = control KR "rate" 1
>         gl = control KR "glide" 0
>         gt = control KR "gate" 1
>         lr = control KR "loopRel" 0
>         sp = control KR "startPos" 0
>         sl = control KR "startLoop" 0
>         el = control KR "endLoop" 0
>         ip = control KR "ipol" 2
>         rt' = lag rt gl * bufRateScale KR bf
>         e = let d = envADSR 0.1 0.2 1 2 1 (EnvNum (-4)) 0
>             in envGen AR gt 1 0 1 RemoveSynth d
>         s = E.loopBuf 1 AR bf rt' (gt + lr) sp sl el ip
>     in out ou (s * e)

> lb0s :: Synthdef
> lb0s = synthdef "lb0" lb0

    ld 0
    withSC3 (async (d_recv lb0s))

    import Sound.OSC {- hosc -}
    let send = sendMessage
    withSC3 (send (s_new "lb0" 3000 AddToTail 1 [("bufnum",0),("startLoop",5000),("endLoop",15000)]))

    withSC3 (send (n_set1 3000 "rate" (-1))) -- backwards
    withSC3 (send (n_set1 3000 "rate" 1)) -- forwards
    withSC3 (send (n_set 3000 [("startLoop",11000),("endLoop",13000)])) -- change loop points
    withSC3 (send (n_set1 3000 "glide" 5)) -- 5 second glide
    withSC3 (send (n_set1 3000 "rate" 2)) -- up an octave
    withSC3 (send (n_set1 3000 "rate" (-1))) -- backwards
    withSC3 (send (n_set1 3000 "rate" 1)) -- back to normal
    withSC3 (send (n_set1 3000 "ipol" 1)) -- no interpolation
    withSC3 (send (n_set1 3000 "ipol" 2)) -- linear interpolation
    withSC3 (send (n_set1 3000 "ipol" 4)) -- cubic interpolation
    withSC3 (send (n_set1 3000 "gate" 0)) -- release gate to hear post-loop

    withSC3 (send (s_new "lb0" 3000 AddToTail 1 [("bufnum",0),("startLoop",5000),("endLoop",15000)]))
    withSC3 (send (n_set 3000 [("loopRel",1),("gate",0)])) -- release instrument without post-loop
