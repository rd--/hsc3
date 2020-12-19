-- loopBuf
let bf = control KR "bufnum" 0
    rt = control KR "rate" 1
    gl = control KR "glide" 0
    gt = control KR "gate" 1
    lr = control KR "loopRel" 0
    sp = control KR "startPos" 0
    sl = control KR "startLoop" 0 -- FRAME
    el = control KR "endLoop" 0 -- FRAME
    ip = control KR "ipol" 2
    am = control KR "amp" 0.1
    rt' = lag rt gl * bufRateScale KR bf
    e = let d = envADSR 0.1 0.2 1 2 1 (EnvNum (-4)) 0
        in envGen AR gt 1 0 1 RemoveSynth d
    s = X.loopBuf 1 AR bf rt' (gt + lr) sp sl el ip
in s * e * am

---- ; read audio file into memory
let fn = "/home/rohan/rd/j/2019-04-21/FAIRLIGHT/IIX/REEDS/clarmdhi.snd"
withSC3 (async (b_allocRead 0 fn 0 0))

---- ; send control messages
import Sound.OSC {- hosc -}
withSC3 (sendMessage (n_set 1 [("startLoop",5376),("endLoop",5504)]))
withSC3 (sendMessage (n_set1 1 "amp" 0.15)) -- louder
withSC3 (sendMessage (n_set1 1 "rate" (-1))) -- backwards
withSC3 (sendMessage (n_set1 1 "rate" 1)) -- forwards
withSC3 (sendMessage (n_set 1 [("startLoop",11000),("endLoop",11)])) -- change loop points
withSC3 (sendMessage (n_set 1 [("startLoop",5000),("endLoop",15000)])) -- change loop points
withSC3 (sendMessage (n_set1 1 "glide" 5)) -- 5 second glide
withSC3 (sendMessage (n_set1 1 "rate" 2)) -- up an octave
withSC3 (sendMessage (n_set1 1 "rate" (-1))) -- backwards
withSC3 (sendMessage (n_set1 1 "rate" 1)) -- back to normal
withSC3 (sendMessage (n_set1 1 "ipol" 1)) -- no interpolation
withSC3 (sendMessage (n_set1 1 "ipol" 2)) -- linear interpolation
withSC3 (sendMessage (n_set1 1 "ipol" 4)) -- cubic interpolation
withSC3 (sendMessage (n_set1 1 "gate" 0)) -- release gate to hear post-loop
withSC3 (sendMessage (n_set 1 [("loopRel",1),("gate",0)]) -- release instrument without post-loop
