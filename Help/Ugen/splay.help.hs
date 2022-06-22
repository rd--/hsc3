-- splay
splay (sinOsc ar (mce [110,220,440,880]) 0 * 0.1) 1 1 0 True

-- splay
mix (splay (pan2 (sinOsc ar (mce [110,220,440,880]) 0) 0 0.075) 1 1 0 True)

-- splay ; composite UGen ; inArray spread=1 level=1 center=0 levelComp=true ; mouse control
let i = 6
    r = map (\e -> randId e 10 20) (take i ['α'..])
    n = lfNoise2Id 'β' kr (mce r)
    x = mouseX kr (-1) 1 Linear 0.1
    y = mouseY kr 1 0 Linear 0.1
    ci = constant . fromIntegral
    f = mce [1 .. ci i] + 3 * 100
    o = sinOsc ar (n * 200 + f) 0
in splay o y 0.2 x True

-- splay ; single channel input
let x = mouseX kr (-1) 1 Linear 0.1
    y = mouseY kr 1 0 Linear 0.1
    o = sinOsc ar (lfNoise2Id 'α' kr 6 * 9 + 440) 0 * 0.1
in splay o y 0.2 x True

-- splay ; n_set control
let i = 10
    s = control kr "spread" 1
    l = control kr "level" 0.2
    c = control kr "center" 0
    r = map (\e -> randId e 10 20) (take i ['α'..])
    ci = constant . fromIntegral
    f = mce [1 .. ci i] + 3 * 100
    n = lfNoise2Id 'β' kr (mce r) * 200 + f
in splay (sinOsc ar n 0) s l c True

---- ; n_set control
import Sound.OSC {- hosc -}
withSc3 (sendMessage (n_set (-1) [("spread",1),("center",0)])) -- full stereo
withSc3 (sendMessage (n_set (-1) [("spread",0.5),("center",0)])) -- less wide
withSc3 (sendMessage (n_set (-1) [("spread",0),("center",0)])) -- mono center
withSc3 (sendMessage (n_set (-1) [("spread",0.5),("center",0.5)])) -- from center to right
withSc3 (sendMessage (n_set (-1) [("spread",0),("center",-1)])) -- all left
