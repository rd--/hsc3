    mapM_ Sound.SC3.UGen.DB.ugenSummary ["LoopBuf","PlayBuf"]

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

Read audio file into memory

> fn_01 = "/home/rohan/rd/j/2019-04-21/FAIRLIGHT/IIX/REEDS/clarmdhi.snd"

> ld fn b = withSC3 (async (b_allocRead b fn 0 0))

Simple sampler instrument

> lb0 =
>     let ou = control KR "out" 0
>         bf = control KR "bufnum" 0
>         rt = control KR "rate" 1
>         gl = control KR "glide" 0
>         gt = control KR "gate" 1
>         lr = control KR "loopRel" 0
>         sp = control KR "startPos" 0
>         sl = control KR "startLoop" 0 -- FRAME
>         el = control KR "endLoop" 0 -- FRAME
>         ip = control KR "ipol" 2
>         am = control KR "amp" 1
>         rt' = lag rt gl * bufRateScale KR bf
>         e = let d = envADSR 0.1 0.2 1 2 1 (EnvNum (-4)) 0
>             in envGen AR gt 1 0 1 RemoveSynth d
>         s = X.loopBuf 1 AR bf rt' (gt + lr) sp sl el ip
>     in out ou (s * e * am)

> lb0s :: Synthdef
> lb0s = synthdef "lb0" lb0

    ld fn_01 0
    withSC3 (async (d_recv lb0s))

    import Sound.OSC {- hosc -}
    let send = sendMessage
    let run = withSC3 . send

    audition (sinOsc AR (midiCPS 69) 0 * 0.2)
    run (s_new "lb0" 3000 AddToTail 1 [("bufnum",0),("amp",0.1),("startLoop",5376),("endLoop",5504)])

    run (n_set1 3000 "amp" 0.25) -- louder
    run (n_set1 3000 "rate" (-1)) -- backwards
    run (n_set1 3000 "rate" 1) -- forwards
    run (n_set 3000 [("startLoop",11000),("endLoop",13000)]) -- change loop points
    run (n_set1 3000 "glide" 5) -- 5 second glide
    run (n_set1 3000 "rate" 2) -- up an octave
    run (n_set1 3000 "rate" (-1)) -- backwards
    run (n_set1 3000 "rate" 1) -- back to normal
    run (n_set1 3000 "ipol" 1) -- no interpolation
    run (n_set1 3000 "ipol" 2) -- linear interpolation
    run (n_set1 3000 "ipol" 4) -- cubic interpolation
    run (n_set1 3000 "gate" 0) -- release gate to hear post-loop

    run (s_new "lb0" 3000 AddToTail 1 [("bufnum",0),("startLoop",5000),("endLoop",15000)])
    run (n_set 3000 [("loopRel",1),("gate",0)]) -- release instrument without post-loop
