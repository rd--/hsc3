-- f0 ; https://twitter.com/redFrik/status/1210118661022867458
let b = mce (map recip [2 .. 7])
    d = 1 / 48
    g = cos (cos 0)
    l = mce [0,-16,0,-16,0,0,-16,0,-16,0,-16,0,0,0,-16,-16,47,0,-16,0,-16,0,-16,0,0,-16,0,0,-16,0,0,0,0,0,-5,-16,-16,-15]
    f = duty ar b 0 DoNothing (dseq dinf (l + 48))
    o = sinOscFB ar f (max (sinOsc ar d 0) 0) * sin (cos 0)
    c = sinOsc ar 0 (pi ** o)
    p = pitchShift c g (recip b) d d * b
in leakDC (splay (c + p) 1 1 0 True) 0.995 * g

-- f0 ; https://twitter.com/redFrik/status/1210118661022867458 ; id
let b = mce (map recip [2 .. 7])
    d = 1 / 48
    g = cos (cos 0)
    l = mce [0,-16,0,-16,0,0,-16,0,-16,0,-16,0,0,0,-16,-16,47,0,-16,0,-16,0,-16,0,0,-16,0,0,-16,0,0,0,0,0,-5,-16,-16,-15]
    f = duty ar b 0 DoNothing (dseqId 'α' dinf (l + 48))
    o = sinOscFB ar f (max (sinOsc ar d 0) 0) * sin (cos 0)
    c = sinOsc ar 0 (pi ** o)
    p = pitchShift c g (recip b) d d * b
in leakDC (splay (c + p) 1 1 0 True) 0.995 * g
