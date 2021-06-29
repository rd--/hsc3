-- loopBuf
let bf = control kr "bufnum" 0
    rt = control kr "rate" 1
    gl = control kr "glide" 0
    gt = control kr "gate" 1
    lr = control kr "loopRel" 0
    sp = control kr "startPos" 0
    sl = control kr "startLoop" 0 -- FRAME
    el = control kr "endLoop" 0 -- FRAME
    ip = control kr "ipol" 2
    am = control kr "amp" 0.1
    rt' = lag rt gl * bufRateScale kr bf
    e = let d = envADSR 0.1 0.2 1 2 1 (EnvNum (-4)) 0
        in envGen ar gt 1 0 1 RemoveSynth d
    s = X.loopBuf 1 ar bf rt' (gt + lr) sp sl el ip
in s * e * am

---- ; read audio file into memory
let fn = "/home/rohan/rd/j/2019-04-21/FAirLIGHT/IIX/REEDS/clarmdhi.snd"
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
