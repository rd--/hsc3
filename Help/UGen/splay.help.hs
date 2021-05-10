-- splay
splay (sinOsc AR (mce [110,220,440,880]) 0 * 0.1) 1 1 0 True

-- splay
mix (splay (pan2 (sinOsc AR (mce [110,220,440,880]) 0) 0 0.075) 1 1 0 True)

-- splay ; composite UGen ; inArray spread=1 level=1 center=0 levelComp=true ; mouse control
let i = 6
    r = map (\e -> rand e 10 20) (take i ['α'..])
    n = lfNoise2 'β' KR (mce r)
    x = mouseX KR (-1) 1 Linear 0.1
    y = mouseY KR 1 0 Linear 0.1
    ci = constant . fromIntegral
    f = mce [1 .. ci i] + 3 * 100
    o = sinOsc AR (n * 200 + f) 0
in splay o y 0.2 x True

-- splay ; single channel input
let x = mouseX KR (-1) 1 Linear 0.1
    y = mouseY KR 1 0 Linear 0.1
    o = sinOsc AR (lfNoise2 'α' KR 6 * 9 + 440) 0 * 0.1
in splay o y 0.2 x True

-- splay ; n_set control
let i = 10
    s = control KR "spread" 1
    l = control KR "level" 0.2
    c = control KR "center" 0
    r = map (\e -> rand e 10 20) (take i ['α'..])
    ci = constant . fromIntegral
    f = mce [1 .. ci i] + 3 * 100
    n = lfNoise2 'β' KR (mce r) * 200 + f
in splay (sinOsc AR n 0) s l c True

---- ; n_set control
import Sound.OSC {- hosc -}
withSC3 (sendMessage (n_set (-1) [("spread",1),("center",0)])) -- full stereo
withSC3 (sendMessage (n_set (-1) [("spread",0.5),("center",0)])) -- less wide
withSC3 (sendMessage (n_set (-1) [("spread",0),("center",0)])) -- mono center
withSC3 (sendMessage (n_set (-1) [("spread",0.5),("center",0.5)])) -- from center to right
withSC3 (sendMessage (n_set (-1) [("spread",0),("center",-1)])) -- all left
